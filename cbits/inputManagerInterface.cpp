#include "MyInputManager.hpp"

extern "C" {
    InputManager * newInputManager( Ogre::RenderWindow *renderWindow );
    void destroyInputManager( InputManager * im );
    void capture( InputManager * im );
//    OIS::Keyboard * newKeyboard(OIS::InputManager * im);
//    void captureKeyboard( OIS::Keyboard * kb );
    void copyKeyStates( InputManager * im, char keys[256]);
    int popKeyStack( InputManager * im, unsigned int * keycode, unsigned int * text, int * pressed );
}

InputManager * newInputManager( Ogre::RenderWindow *renderWindow ){
    InputManager * im = new InputManager();
    im->initialize(renderWindow);
    return im;
}

void destroyInputManager( InputManager * im ){
    delete im;
}

void capture( InputManager * im ){
    im->capture();
}

/*************** keyboard **************/
void copyKeyStates( InputManager * im, char keys[256]){
    im->getKeyboard()->copyKeyStates( keys );
}

int popKeyStack( InputManager * im, unsigned int * keycode, unsigned int * text, int * pressed ){
    if (im->keyboardStack.empty())
        return 1;
    keyEvent_t ke = im->keyboardStack.top();
    *keycode = ke.keycode;
    *text    = ke.text;
    *pressed = ke.pressed;
    im->keyboardStack.pop();
    return 0;
}
