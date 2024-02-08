---
title:                "TOMLを扱う方法"
aliases:
- ja/python/working-with-toml.md
date:                  2024-01-26T04:25:42.813247-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOML は、Tom's Obvious, Minimal Language の略で、JSON や YAML に似たデータ直列化フォーマットですが、シンプルさと可読性を目指しています。プログラマーは、書きやすく理解しやすいため、そして Python のようなプログラミング言語のデータ構造にきれいにマッピングできるため、設定ファイルに TOML を使用します。

## 方法：
始める前に、`pip install toml` で `toml` パッケージをインストールしましょう。TOMLファイルを解析してみましょう：

```python
import toml

# 文字列としての例題 TOML コンテンツ
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # 日付のファーストクラス

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# TOML文字列を解析
parsed_toml = toml.loads(toml_string)

# データへのアクセス
print(parsed_toml['owner']['name'])  # 出力: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # 出力: [8001, 8001, 8002]
```

## 深く掘り下げて
TOML は、GitHub の創設者の一人である Tom Preston-Werner によって、よりユーザーフレンドリーな設定ファイルフォーマットとして作られました。それはハッシュテーブルに曖昧さなくマッピングされ、機械によって簡単に解析できるように設計されています。

JSON と比較して、TOML は設定ファイルにとってより読みやすく、コメントをサポートしています。別の代替手段である YAML はよりコンパクトになり得ますが、インデントへの依存や、タブが許されないなどの微妙な問題に人々が躓くことがあります。

実装の詳細については、TOML の値には型があり、文字列、整数、浮動小数点数、ブーリアン、日付時刻、配列、およびテーブルが含まれます。全ては大文字と小文字を区別します。また、TOML は複数行の文字列をサポートし、最新バージョンでは異種型の配列も許容します。

Python では、JSON や YAML ライブラリと同様の API を持つ `toml` ライブラリを使用します。ファイルまたは文字列から TOML を読み込むために `toml.load` と `toml.loads` を、それを書き出すために `toml.dump` と `toml.dumps` を持っています。

## 参照
- 公式 TOML GitHub リポジトリ（仕様について）: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `toml` Python ライブラリ文書: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- TOML の実際の例：Rust のパッケージマネージャー `cargo` や Python のパッケージングツール `poetry` の設定ファイル。
