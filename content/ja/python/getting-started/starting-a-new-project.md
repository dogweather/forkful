---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:27.950806-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.500158-06:00'
model: gpt-4-0125-preview
summary: "Python\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\
  \u59CB\u3081\u308B\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u521D\u3081\u304B\u3089\
  \u69CB\u9020\u3092\u6301\u305F\u305B\u3066\u3001\u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\
  \u304C\u3057\u3084\u3059\u3044\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u3092\u8A2D\
  \u5B9A\u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u304C\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u30C7\u30D0\u30C3\u30B0\u3057\u3084\u3059\u304F\u3001\u7279\u306B\u30D7\
  \u30ED\u30B8\u30A7\u30AF\u30C8\u3084\u4F5C\u696D\u3057\u3066\u3044\u308B\u30C1\u30FC\
  \u30E0\u304C\u6642\u9593\u3068\u3068\u3082\u306B\u6210\u9577\u3059\u308B\u306B\u3064\
  \u308C\u3066\u3001\u5171\u540C\u4F5C\u696D\u304C\u3057\u3084\u3059\u3044\u3088\u3046\
  \u306B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\
  ."
title: "\u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB"
weight: 1
---

## 何を、なぜ？

Pythonで新しいプロジェクトを始めるということは、初めから構造を持たせて、メンテナンスがしやすいフレームワークを設定することについてです。プログラマーは、コードが読みやすく、デバッグしやすく、特にプロジェクトや作業しているチームが時間とともに成長するにつれて、共同作業がしやすいようにするためにこれを行います。

## どうやって：

### 仮想環境の作成
仮想環境は、Pythonプロジェクトが必要とするパッケージを使用するための実行可能ファイルがすべて含まれている自己完結型のディレクトリです。プロジェクト依存関係間の衝突を避けるために、各プロジェクトに対して仮想環境を作成することが望ましいです。標準Pythonライブラリの一部である`venv`モジュールを使用してください。

```shell
# 'myproject'をあなたのプロジェクトの名前に置き換えてください
python3 -m venv myproject-env
```

仮想環境をアクティブにするには：

Windowsの場合：
```shell
myproject-env\Scripts\activate.bat
```

UnixまたはMacOSの場合：
```shell
source myproject-env/bin/activate
```

サンプル出力（出力はOSによって若干異なる場合があります）：
```shell
(myproject-env) $
```

### パッケージのインストール
Pythonのパッケージインストーラーである`pip`を使用して、パッケージをインストール、アップグレード、および削除します。ここでは、HTTPリクエストを行うための人気のあるサードパーティライブラリ`requests`をインストールする方法を示します：

```shell
pip install requests
```

サンプル出力：
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### プロジェクト構造の設定
典型的なPythonプロジェクトは、このような形になるかもしれません：

```
myproject/
│
├── myproject-env/    # 仮想環境
├── docs/             # ドキュメント
├── tests/            # ユニットおよび統合テスト
│   └── __init__.py
├── myproject/        # プロジェクトのソースコード 
│   ├── __init__.py
│   └── main.py
├── setup.py          # プロジェクト設定ファイル
└── README.md         # プロジェクト概要
```

### 最初のプログラムを作成する
`myproject`ディレクトリ内に`main.py`ファイルを作成します。ここにシンプルなプログラムの例を示します：

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```

プログラムを実行する：

```shell
python myproject/main.py
```

サンプル出力：
```shell
Hello, World!
```

### より大きなプロジェクトのためのフレームワークを使用する
特にウェブアプリケーションの場合、DjangoやFlaskのようなフレームワークは非常に貴重です。Flaskをインストールして、シンプルな「Hello, World」ウェブアプリケーションを作成する方法はこちらです：

```shell
pip install Flask
```

次の内容で`app.py`ファイルを作成します：

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Flaskアプリケーションを実行する：

```shell
flask run
```

サンプル出力：
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

ウェブブラウザで`http://127.0.0.1:5000/`にアクセスすると、「Hello, World!」のメッセージが表示されます。
