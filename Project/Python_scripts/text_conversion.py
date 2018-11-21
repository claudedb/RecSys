import os;
import sys;
Path="C:\Users\claudedb\Desktop\Descriptions"
for root, dirs, files in os.walk(Path):
    for filename in files:
        if filename[-5:] == '.html':
            notepad.open(root + "\\" + filename)
            console.write(root + "\\" + filename + "\r\n")
            notepad.runMenuCommand("Encodage", "Convertir en ANSI")
            notepad.save()
            notepad.close()