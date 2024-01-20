---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何となぜ? - What & Why?
新規プロジェクトの立ち上げとは、新たな初期空間をつくり、新しいアイデアや機能を実装することです。これはプログラマがゼロから取り組めるクリエイティブな場所であり、自己表現の一形態であり、またソフトウェア開発における非常に重要なステップとなります。

## 追って - How to:
新規プロジェクトをPythonで開始するには以下のコードが使用できます。クリーンなディレクトリを作成し、サンプルの`hello.py`スクリプトを作ります。

```Python
# 必要なライブラリをインポートします
import os

# 新規プロジェクトのディレクトリを作成します
os.makedirs('my_project')

# ディレクトリ内にサンプルスクリプトを作成します
with open('my_project/hello.py', 'w') as f:
    f.write('print("Hello, World!")')

# Pythonスクリプトを実行してみます
os.system('python my_project/hello.py')
```
出力:
```Python
Hello, World!
```

## ディープダイブ - Deep Dive
新規プロジェクトの立ち上げは、プログラミングの歴史と同じくらい古くから存在します。Pythonはそのアプローチの中でも特にユーザーフレンドリーで、ディレクトリ管理やファイル作成などが容易です。しかし、他の開発環境でも同様のことが可能で、例えばNode.jsでは`npm init`、Rubyでは`bundle init`を使います。詳細な実装に関しては、通常、プロジェクトのニーズに基づくもので、特にPythonにおけるプロジェクトマネジメントツール `venv` や `pipenv` が有用である。

## 参照 - See Also
- [Python os モジュール公式ドキュメント](https://docs.python.org/ja/3/library/os.html)

- [Pythonでのプロジェクト管理ガイド](https://packaging.python.org/tutorials/managing-dependencies/)

- [Pythonのvenv公式ガイド](https://docs.python.org/ja/3/library/venv.html)

- [Python Pipenv公式ガイド](https://pipenv.pypa.io/en/latest/)