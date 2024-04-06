---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:27.950806-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A\u2026"
lastmod: '2024-03-13T22:44:41.500158-06:00'
model: gpt-4-0125-preview
summary: "\u4EEE\u60F3\u74B0\u5883\u306F\u3001Python\u30D7\u30ED\u30B8\u30A7\u30AF\
  \u30C8\u304C\u5FC5\u8981\u3068\u3059\u308B\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\
  \u7528\u3059\u308B\u305F\u3081\u306E\u5B9F\u884C\u53EF\u80FD\u30D5\u30A1\u30A4\u30EB\
  \u304C\u3059\u3079\u3066\u542B\u307E\u308C\u3066\u3044\u308B\u81EA\u5DF1\u5B8C\u7D50\
  \u578B\u306E\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u3067\u3059\u3002\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u4F9D\u5B58\u95A2\u4FC2\u9593\u306E\u885D\u7A81\u3092\u907F\u3051\
  \u308B\u305F\u3081\u306B\u3001\u5404\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306B\u5BFE\
  \u3057\u3066\u4EEE\u60F3\u74B0\u5883\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u304C\
  \u671B\u307E\u3057\u3044\u3067\u3059\u3002\u6A19\u6E96Python\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u306E\u4E00\u90E8\u3067\u3042\u308B`venv`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\
  \u4F7F\u7528\u3057\u3066\u304F\u3060\u3055\u3044."
title: "\u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB"
weight: 1
---

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
