#ifndef InputManager_HPP
#define InputManager_HPP

#include <stack>

#include <OISMouse.h>
#include <OISKeyboard.h>
#include <OISJoyStick.h>
#include <OISInputManager.h>

#include <OgreRenderWindow.h>
 
typedef struct {
    unsigned int keycode;
    unsigned int text;
    int pressedReleased;
} keyEvent_t;

typedef struct {
    int abs;
    int rel;
    int absOnly; // Indicates if this Axis only supports Absolute (ie JoyStick)
} axis_t;

typedef struct {
    axis_t X;
    axis_t Y;
    axis_t Z;
    int buttonId;
    int pressedReleasedMoved;
} mouseEvent_t;

class InputManager : public OIS::KeyListener, public OIS::MouseListener, public OIS::JoyStickListener {
public:
    InputManager( void );
    ~InputManager( void );
 
    void initialize( Ogre::RenderWindow *renderWindow );
    void capture( void );
 
    void setWindowExtents( int width, int height );
 
    OIS::Mouse*    getMouse( void );
    OIS::Keyboard* getKeyboard( void );
    OIS::JoyStick* getJoystick( unsigned int index );
 
    int getNumOfJoysticks( void );
 
    std::stack<keyEvent_t> keyboardStack;
    std::stack<mouseEvent_t> mouseStack;
private:

    bool keyPressed( const OIS::KeyEvent &e );
    bool keyReleased( const OIS::KeyEvent &e );
 
    bool mouseMoved( const OIS::MouseEvent &e );
    bool mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id );
    bool mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id );
 
    bool povMoved( const OIS::JoyStickEvent &e, int pov );
    bool axisMoved( const OIS::JoyStickEvent &e, int axis );
    bool sliderMoved( const OIS::JoyStickEvent &e, int sliderID );
    bool buttonPressed( const OIS::JoyStickEvent &e, int button );
    bool buttonReleased( const OIS::JoyStickEvent &e, int button );
 
    OIS::Mouse        *mMouse;
    OIS::Keyboard     *mKeyboard;
    OIS::InputManager *mInputSystem;
 
    std::vector<OIS::JoyStick*> mJoysticks;
    std::vector<OIS::JoyStick*>::iterator itJoystick;
    std::vector<OIS::JoyStick*>::iterator itJoystickEnd;
};
#endif
