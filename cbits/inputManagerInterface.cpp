#include "MyInputManager.hpp"

extern "C" {
    // input manager
    InputManager * newInputManager( Ogre::RenderWindow *renderWindow );
    void destroyInputManager( InputManager * im );
    void capture( InputManager * im );
    // keyboard
    void copyKeyStates( InputManager * im, char keys[256]);
    int popKeyStack( InputManager * im, unsigned int * keycode, unsigned int * text, int * pressed );
    // mouse
    void copyMouseState( InputManager * im, int axes[9], int * width, int * height, int * buttons );
    int popMouseStack( InputManager * im, int axes[9], int * buttonId, int * pressedReleasedMoved );
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
    *pressed = ke.pressedReleased;
    im->keyboardStack.pop();
    return 0;
}


/*************** mouse **************/
void copyMouseState( InputManager * im, int axes[9], int * width, int * height, int * buttons ){
    OIS::MouseState ms = im->getMouse()->getMouseState();
    axes[0] = ms.X.abs;
    axes[1] = ms.X.rel;
    axes[2] = ms.X.absOnly;
    axes[3] = ms.Y.abs;
    axes[4] = ms.Y.rel;
    axes[5] = ms.Y.absOnly;
    axes[6] = ms.Z.abs;
    axes[7] = ms.Z.rel;
    axes[8] = ms.Z.absOnly;
    *width = ms.width;
    *height = ms.height;
    *buttons = ms.buttons;
}

int popMouseStack( InputManager * im, int axes[9], int * buttonId, int * pressedReleasedMoved ){
    if (im->mouseStack.empty())
        return 1;
    mouseEvent_t me = im->mouseStack.top();
    axes[0] = me.X.abs;
    axes[1] = me.X.rel;
    axes[2] = me.X.absOnly;
    axes[3] = me.Y.abs;
    axes[4] = me.Y.rel;
    axes[5] = me.Y.absOnly;
    axes[6] = me.Z.abs;
    axes[7] = me.Z.rel;
    axes[8] = me.Z.absOnly;
    *buttonId = me.buttonId;
    *pressedReleasedMoved = me.pressedReleasedMoved;
    im->mouseStack.pop();
    return 0;
}
