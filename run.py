from flask import (
        Flask, 
        render_template,
        g,
        url_for,
        session,
        request,
        redirect
        )
from flask_sqlalchemy import SQLAlchemy
from flask_bootstrap import Bootstrap

app = Flask(__name__)
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///db.sqlite3'

db = SQLAlchemy(app)

class User(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(50))
    password = db.Column(db.String(50))

@app.before_request
def before_request():
    g.user = None

    if 'user_id' in session:
        user = User.query.filter_by(id=session['user_id']).first()
        if user.username in globals():
            redirect(url_for('login'))
        g.username = user.username


app.secret_key = "HJKSDUFYBOuidgfos7"
Bootstrap(app)


@app.route('/login', methods=['POST', 'GET'])
def login():
    if request.method == 'POST':
        session.pop('user_id', None)
        username = request.form['username']
        password = request.form['password']

        user = User.query.filter_by(username=username).first()

        if user and user.password == password:
            session['user_id'] = user.id
            return redirect(url_for('home'))
    return render_template('login.html')

@app.route('/')
def home():
    try:
        if g.username:
            return render_template('home.html')
    except:
        return redirect(url_for('login'))

@app.route('/game')
def unity_player():
    return render_template('unity.html')


if __name__ == "__main__":
    app.run(host="0.0.0.0", port="80", debug=True)
