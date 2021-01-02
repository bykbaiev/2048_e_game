require('./index.html');

const Elm = require('./Main.elm').Elm;

const GAME_STATE_KEY = 'GAME_STATE';

const app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: JSON.parse(localStorage.getItem(GAME_STATE_KEY) || null)
});

app.ports.storeGameState.subscribe((state) => {
    localStorage.setItem(GAME_STATE_KEY, JSON.stringify(state));
});

