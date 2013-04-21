#include "MyInputManager.hpp"
#include <cstdio>
 
InputManager::~InputManager( void ) {
    if( mMouse ) {
        mInputSystem->destroyInputObject( mMouse );
        mMouse = 0;
    }

    if( mKeyboard ) {
        mInputSystem->destroyInputObject( mKeyboard );
        mKeyboard = 0;
    }

    if( mJoysticks.size() > 0 ) {
        itJoystick    = mJoysticks.begin();
        itJoystickEnd = mJoysticks.end();
        for(; itJoystick != itJoystickEnd; ++itJoystick ) {
            mInputSystem->destroyInputObject( *itJoystick );
        }

        mJoysticks.clear();
    }

    // If you use OIS1.0RC1 or above, uncomment this line
    // and comment the line below it
    OIS::InputManager::destroyInputSystem( mInputSystem );
    //mInputSystem->destroyInputSystem();
    mInputSystem = 0;
}
 
InputManager::InputManager( void ) :
    mMouse( 0 ),
    mKeyboard( 0 ),
    mInputSystem( 0 ) {
}
 
void InputManager::initialize( Ogre::RenderWindow *renderWindow ) {
    // Setup basic variables
    OIS::ParamList paramList;    
    size_t windowHnd = 0;
    std::ostringstream windowHndStr;

    // Get window handle
    renderWindow->getCustomAttribute( "WINDOW", &windowHnd );

    // Fill parameter list
    windowHndStr << (unsigned int) windowHnd;
    paramList.insert( std::make_pair( std::string( "WINDOW" ), windowHndStr.str() ) );

    // Create inputsystem
    mInputSystem = OIS::InputManager::createInputSystem( paramList );

    // If possible create a buffered keyboard
    // (note: if below line doesn't compile, try:  if (mInputSystem->getNumberOfDevices(OIS::OISKeyboard) > 0) {
    //if( mInputSystem->numKeyboards() > 0 ) {
    if (mInputSystem->getNumberOfDevices(OIS::OISKeyboard) > 0) {
        mKeyboard = static_cast<OIS::Keyboard*>( mInputSystem->createInputObject( OIS::OISKeyboard, true ) );
        mKeyboard->setEventCallback( this );
    }

    // If possible create a buffered mouse
    // (note: if below line doesn't compile, try:  if (mInputSystem->getNumberOfDevices(OIS::OISMouse) > 0) {
    //if( mInputSystem->numMice() > 0 ) {
    if (mInputSystem->getNumberOfDevices(OIS::OISMouse) > 0) {
        mMouse = static_cast<OIS::Mouse*>( mInputSystem->createInputObject( OIS::OISMouse, true ) );
        mMouse->setEventCallback( this );

        // Get window size
        unsigned int width, height, depth;
        int left, top;
        renderWindow->getMetrics( width, height, depth, left, top );

        // Set mouse region
        this->setWindowExtents( width, height );
    }

    // If possible create all joysticks in buffered mode
    // (note: if below line doesn't compile, try:  if (mInputSystem->getNumberOfDevices(OIS::OISJoyStick) > 0) {
    //if( mInputSystem->numJoySticks() > 0 ) {
    if (mInputSystem->getNumberOfDevices(OIS::OISJoyStick) > 0) {
        //mJoysticks.resize( mInputSystem->numJoySticks() );
        mJoysticks.resize( mInputSystem->getNumberOfDevices(OIS::OISJoyStick) );

        itJoystick    = mJoysticks.begin();
        itJoystickEnd = mJoysticks.end();
        for(; itJoystick != itJoystickEnd; ++itJoystick ) {
            (*itJoystick) = static_cast<OIS::JoyStick*>( mInputSystem->createInputObject( OIS::OISJoyStick, true ) );
            (*itJoystick)->setEventCallback( this );
        }
    }
}
 
void InputManager::capture( void ) {
    // Need to capture / update each device every frame
    if( mMouse ) {
        mMouse->capture();
    }
 
    if( mKeyboard ) {
        mKeyboard->capture();
    }
 
    if( mJoysticks.size() > 0 ) {
        itJoystick    = mJoysticks.begin();
        itJoystickEnd = mJoysticks.end();
        for(; itJoystick != itJoystickEnd; ++itJoystick ) {
            (*itJoystick)->capture();
        }
    }
}
 
void InputManager::setWindowExtents( int width, int height ) {
    // Set mouse region (if window resizes, we should alter this to reflect as well)
    const OIS::MouseState &mouseState = mMouse->getMouseState();
    mouseState.width  = width;
    mouseState.height = height;
}
 
OIS::Mouse* InputManager::getMouse( void ) {
    return mMouse;
}
 
OIS::Keyboard* InputManager::getKeyboard( void ) {
    return mKeyboard;
}
 
OIS::JoyStick* InputManager::getJoystick( unsigned int index ) {
    // Make sure it's a valid index
    if( index < mJoysticks.size() ) {
        return mJoysticks[ index ];
    }
 
    return 0;
}
 
int InputManager::getNumOfJoysticks( void ) {
    // Cast to keep compiler happy ^^
    return (int) mJoysticks.size();
}
 
bool InputManager::keyPressed( const OIS::KeyEvent &e ) {
    keyEvent_t ke = {
        .keycode = e.key,
        .text = e.text,
        .pressed = 1,
    };
    keyboardStack.push(ke);
    return true;
}
 
bool InputManager::keyReleased( const OIS::KeyEvent &e ) {
    keyEvent_t ke = {
        .keycode = e.key,
        .text = e.text,
        .pressed = 0,
    };
    keyboardStack.push(ke);
    return true;
}

bool InputManager::mouseMoved( const OIS::MouseEvent &e ) {
    std::cout << "mouse moved, yay\n";
    return true;
}
 
bool InputManager::mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id ) {
    std::cout << "mouse pressed, yay\n";
    return true;
}
 
bool InputManager::mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id ) {
    std::cout << "mouse released, yay\n";
    return true;
}
 
bool InputManager::povMoved( const OIS::JoyStickEvent &e, int pov ) {
    std::cout << "povMoved, yay\n";
    return true;
}
 
bool InputManager::axisMoved( const OIS::JoyStickEvent &e, int axis ) {
    std::cout << "axisMoved, yay\n";
    return true;
}
 
bool InputManager::sliderMoved( const OIS::JoyStickEvent &e, int sliderID ) {
    std::cout << "sliderMoved, yay\n";
    return true;
}
 
bool InputManager::buttonPressed( const OIS::JoyStickEvent &e, int button ) {
    std::cout << "buttonPressed, yay\n";
    return true;
}
 
bool InputManager::buttonReleased( const OIS::JoyStickEvent &e, int button ) {
    std::cout << "buttonReleased, yay\n";
    return true;
}
