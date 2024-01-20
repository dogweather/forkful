---
title:                "テキストファイルの作成"
html_title:           "Python: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
テキストファイルを書くことは、テキストをファイルに保存することです。プログラマーがこれを行う理由は、データを永続的に保存したり、エクスポートしたり、他のプログラムと共有したりするためです。

## 方法：
```Python
# テキストファイルを作成して書き込む
with open('new_file.txt', 'w') as file:
    file.write('これはテキストファイルに書かれたテキストです。')

# テキストファイルを読み取る
with open('existing_file.txt', 'r') as file:
    text = file.read()
    print(text)

# テキストファイルを追記する
with open('existing_file.txt', 'a') as file:
    file.write('これは追加されたテキストです。')
```

## 詳細：
テキストファイルを書くことは、情報を古くからの方法で保存する一般的な方法です。他の方法としては、データベースやプログラミング言語固有の形式を使用する方法があります。テキストファイルを書くには、「open」関数を使用し、モードを指定してファイルを開きます。また、ファイルを読み取る際には、ファイルを閉じるまでメモリ上にデータが保持されることに注意してください。

## 関連リンク：
- [Pythonのopen関数について（公式ドキュメント）](https://docs.python.org/ja/3/library/functions.html#open)