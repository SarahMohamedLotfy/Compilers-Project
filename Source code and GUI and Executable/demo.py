import sys
import os
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QPlainTextEdit, QHBoxLayout, QVBoxLayout
import subprocess
from subprocess import PIPE, Popen
from pyreadline import Readline

class MyApp(QWidget):
    def __init__(self):
        super().__init__()
        self.window_width, self.window_height = 1200, 900
        self.setMinimumSize(self.window_width, self.window_height)

        self.setWindowTitle('Command Line App')
        layout = QVBoxLayout()
        self.setLayout(layout)

        self.editorCommand = QPlainTextEdit()
        layout.addWidget(self.editorCommand, 8)

        self.editorOutput = QPlainTextEdit()
        layout.addWidget(self.editorOutput, 2)

        buttonLayout = QHBoxLayout()
        layout.addLayout(buttonLayout)

        self.button_run = QPushButton('&Run', clicked=self.runCommand)
        buttonLayout.addWidget(self.button_run)

        self.button_clear = QPushButton('&Clear', clicked=lambda: self.editorOutput.clear())
        buttonLayout.addWidget(self.button_clear)

        self.editorCommand.insertPlainText('')
        
        '''
        flex lex.l
        bison -dy yacc.y
        gcc lex.yy.c y.tab.c -o compiler
        compiler
        '''
        # p = os.popen('flex lex.l')
        # p = os.popen('bison -dy yacc.y')
        # p = os.popen('gcc lex.yy.c y.tab.c -o compiler')

    def runCommand(self):
        command_line = self.editorCommand.toPlainText().strip()
        self.editorOutput.clear()
        
        # commands = command_line.split('\n')
        # for command_line in commands:
        if '.cpp' in command_line:
            file1 = open(command_line,"r")
            command_line = file1.read()
            file1.close()
        
        p = os.popen('compiler','w')
        p.write(command_line)
        
        if p:
            p.write('exit;\n')
            p.close()
            # po = Popen(['compiler',command_line], stdout=PIPE, stdin=PIPE, stderr=PIPE)
            
            # (output, err) = po.communicate()
            

            readline = Readline()
            for i in range(readline.get_current_history_length()):
                self.editorOutput.insertPlainText (readline.get_history_item(i + 1))
            # result = readline.get_history_item()
            self.editorOutput.insertPlainText (str(readline.get_current_history_length()))



if __name__ == '__main__':
    # don't auto scale when drag app to a different monitor.
    # QApplication.setAttribute(Qt.HighDpiScaleFactorRoundingPolicy.PassThrough)
    
    app = QApplication(sys.argv)
    app.setStyleSheet('''
        QWidget {
            font-size: 30px;
        }
    ''')
    
    myApp = MyApp()
    myApp.show()

    try:
        sys.exit(app.exec_())
    except SystemExit:
        print('Closing Window...')

