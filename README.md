# tic-tac-toe-bot

This bot is haskell binary (executable) which:
- Reads opponent's messages from standart input (stdin)
- Writes messages for opponent to standart output (stdout)
- Writes debug information of a game (e.g. "Game state: (1,1,X), (0,0,O). My move (2,2,X)") in a human readable form to standart error output (stderr).
- A single program launch processes a single move, i.e. program halts when message is written to stdout.
- Game status (from the perspective of a particular bot) is reported using exit codes (see table below)
- Each bot is be able to play as player 'X' and as a player 'O'. 'X' starts a game and expects `*` message in stdin. `X` or `O` are passed as executable parameters.
- Programs do no not store any intermediate state and fully rely on messages.
- Bot tries to win a game. A tie is good enough result. A win is a perfect result.

| Exit Code | Meaning                                                                                          |
| --------- | ------------------------------------------------------------------------------------------------ |
| 0         | All good, we are in the middle of a game                                                         |
| 10        | I just performed the last move, I won                                                            |
| 12        | I just performed the last move, a draw                                                           |
| 20        | I cannot perform any moves because game is already ended (board is full or there is a winner)    |
| 100       | Incoming message is malformed (bad syntax)                                                       |
| 101       | Incoming message is semanticallly invalid (e.g. 2 moves to a same cell or game is already ended) |

`message` is a whole history of a game encoded in Bencode. The non empty list is represented as a recursive map `{"last": MOVE_DATA, "prev": { "last": MOVE_DATA, "prev": ...} }`. `MOVE_DATA` is a dictionary `{"data": [X, Y, V]}`, where `X` and `Y` are integers while `V` is a char: 'X' or 'O'.  The "deepest" entry (the first move) has no "prev" key. http://tic-tac-toe.homedir.eu/arbitrary/3/76 will generate you an example message or you can look at `Task3Message.hs` file.

All matrices are 3x3 size.

All these std(in|out|err) work well on local machine. But it can be run remotely as well. To achieve that [this](https://git.mif.vu.lt/vipo/tic-tac-toe-runner) project must be used. It provides 2 programs:
`tic-tac-toe-offender` and `tic-tac-toe-defender`.
The one who starts a game runs `tic-tac-toe-offender 76 $PATH_TO_YOUR_BOT_EXECUTABLE` while another one runs
`tic-tac-toe-defender 76 $PATH_TO_YOUR_BOT_EXECUTABLE`.

Some additional notes:
- When a bot puts a last `X` or `O` (win or 9th move) on a board, the board is sent to a competitor
- When a bot gets a board with ended games, it just exits (see exit codes)
- Runner has 4 colors of output: Red - errors, you have to eliminate those. Blue - helpful messages from the runner, Green - stdin, stdout, sterr of your bot, Default color (probably white) - network exceptions, should be fixed on next run. 
