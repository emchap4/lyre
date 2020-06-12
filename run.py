from flask import Flask, render_template
from flask_bootstrap import Bootstrap

app = Flask(__name__)
Bootstrap(app)

@app.route('/')
def home():
    return render_template('home.html')

@app.route('/unity')
def unity_player():
    return render_template('unity.html')


if __name__ == "__main__":
    app.run(debug=True)
