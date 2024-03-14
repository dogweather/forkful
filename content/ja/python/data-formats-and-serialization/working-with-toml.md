---
date: 2024-01-26 04:25:42.813247-07:00
description: "TOML \u306F\u3001Tom's Obvious, Minimal Language \u306E\u7565\u3067\u3001\
  JSON \u3084 YAML \u306B\u4F3C\u305F\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3067\u3059\u304C\u3001\u30B7\u30F3\u30D7\u30EB\u3055\u3068\
  \u53EF\u8AAD\u6027\u3092\u76EE\u6307\u3057\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u66F8\u304D\u3084\u3059\u304F\u7406\u89E3\u3057\
  \u3084\u3059\u3044\u305F\u3081\u3001\u305D\u3057\u3066 Python\u2026"
lastmod: '2024-03-13T22:44:41.531321-06:00'
model: gpt-4-0125-preview
summary: "TOML \u306F\u3001Tom's Obvious, Minimal Language \u306E\u7565\u3067\u3001\
  JSON \u3084 YAML \u306B\u4F3C\u305F\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3067\u3059\u304C\u3001\u30B7\u30F3\u30D7\u30EB\u3055\u3068\
  \u53EF\u8AAD\u6027\u3092\u76EE\u6307\u3057\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u66F8\u304D\u3084\u3059\u304F\u7406\u89E3\u3057\
  \u3084\u3059\u3044\u305F\u3081\u3001\u305D\u3057\u3066 Python\u2026"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
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
