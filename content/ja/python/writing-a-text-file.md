---
title:    "Python: テキストファイルの書き方"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

#Why
テキストファイルを作成することには、重要な理由があります。テキストファイルは、コンピューターで情報やデータを保存するためのもっとも基本的な方法です。Pythonを使ってテキストファイルを作成することで、データを簡単に保存したり、共有したりすることができます。

#How to
Pythonを使用してテキストファイルを作成する方法はとても簡単です。まずはファイルを開きます。
```
file = open("textfile.txt", "w")
```
次に、ファイルに書き込むテキストを定義します。
```
text = "これはテストのテキストです。"
```
最後に、ファイルにテキストを書き込みます。
```
file.write(text)
```
これで、"textfile.txt"という名前のファイルが作成され、"これはテストのテキストです。"という内容が書き込まれます。

データを追加する場合、ファイルを再び開いて、"a"(追加モード)で書き込みます。
```
file = open("textfile.txt", "a")
```
追加するテキストを定義し、ファイルに書き込みます。
```
new_text = "これは新しいテキストです。"
file.write(new_text)
```
これで、既存のテキストに新しいテキストが追加されます。

#Deep Dive
テキストファイルの作成には、"w"(書き込みモード)と"a"(追加モード)の2つのオプションがあります。"w"モードでは、ファイルが存在しない場合は新しいファイルを作成し、ファイルがすでに存在する場合はファイルを上書きします。一方、"a"モードでは、ファイルが存在しない場合は新しいファイルを作成し、ファイルがすでに存在する場合は既存のファイルの末尾に追加します。

また、ファイルを閉じることも忘れないようにしましょう。ファイルを閉じることで、データが正しく保存され、メモリを節約することができます。

```
file.close()
```

#See Also
[Tech with Tim: Writing to a File in Python](https://www.youtube.com/watch?v=LJTaPaFGmM4)<br>
[Tutorials Point: Python File I/O](https://www.tutorialspoint.com/python/python_files_io.htm)<br>
[Real Python: Reading and Writing Files in Python](https://realpython.com/read-write-files-python/)