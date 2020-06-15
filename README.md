# lyre

## Usage

Make venv and install requirements

```bash
python3 -m venv .env && source .env/bin/activate && pip3 install -r requirements.txt
```

or just install requirements directly

```bash
pip3 install -r requirements.txt
```

Run Flask application

```bash
python3 run.py
```

----
## Recompile Elm into JS

```bash
elm make src/Main.elm --output static/game.js
```
