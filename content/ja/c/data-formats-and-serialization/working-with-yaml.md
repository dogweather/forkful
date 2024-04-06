---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:38.019170-07:00
description: ''
lastmod: '2024-04-05T22:38:42.312862-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u3067YAML\u3092\u6271\u3046\u306B\u306F\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\u3002\u306A\u305C\u306A\
  \u3089\u3001\u6A19\u6E96C\u30E9\u30A4\u30D6\u30E9\u30EA\u306FYAML\u306E\u89E3\u6790\
  \u3084\u76F4\u5217\u5316\u306E\u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u30B5\u30DD\
  \u30FC\u30C8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u306A\u3044\u304B\u3089\u3067\u3059\
  \u3002C\u8A00\u8A9E\u3067\u6700\u3082\u4EBA\u6C17\u306E\u3042\u308BYAML\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306E\u4E00\u3064\u304C`libyaml`\u3067\u3042\u308A\u3001\u89E3\
  \u6790\u3068\u51FA\u529B\u306E\u305F\u3081\u306E\u4F4E\u30EC\u30D9\u30EB\u3068\u9AD8\
  \u30EC\u30D9\u30EB\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u4E21\u65B9\
  \u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001`libyaml`\u3092\
  \u4F7F\u7528\u3057\u3066\u7C21\u5358\u306AYAML\u30D5\u30A1\u30A4\u30EB\u3092\u89E3\
  \u6790\u3059\u308B\u65B9\u6CD5\u306E\u4F8B\u3067\u3059\uFF1A **\u307E\u305A**\u3001\
  `libyaml`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002Unix\u7CFB\u306E\u30B7\
  \u30B9\u30C6\u30E0\u306B\u3044\u308B\u5834\u5408\u3001\u901A\u5E38\u306F\u30D1\u30C3\
  \u30B1\u30FC\u30B8\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\u7D4C\u7531\u3067\u30A4\u30F3\
  \u30B9\u30C8\u30FC\u30EB\u3067\u304D\u307E\u3059\u3002\u4F8B\u3048\u3070Ubuntu\u3067\
  \u306F\uFF1A."
title: "YAML\u3068\u306E\u4F5C\u696D"
weight: 41
---

## 方法：
C言語でYAMLを扱うにはライブラリが必要です。なぜなら、標準CライブラリはYAMLの解析や直列化のための直接的なサポートを提供していないからです。C言語で最も人気のあるYAMLライブラリの一つが`libyaml`であり、解析と出力のための低レベルと高レベルのインターフェイス両方を提供しています。以下は、`libyaml`を使用して簡単なYAMLファイルを解析する方法の例です：

**まず**、`libyaml`ライブラリをインストールする必要があります。Unix系のシステムにいる場合、通常はパッケージマネージャー経由でインストールできます。例えばUbuntuでは：

```bash
sudo apt-get install libyaml-dev
```

**次に**、`config.yaml`という名前の簡単なYAMLファイルを考えます：

```yaml
name: John Doe
age: 29
married: false
```

**以下**は、このYAMLファイルをC言語で解析する基本的な例です：

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("YAML parserの初期化に失敗しました！\n", stderr);

    if (fh == NULL)
        fputs("ファイルを開けませんでした！\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("値: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

この簡単なプログラムは、YAMLファイルを開き、YAMLパーサーを初期化し、ファイルを読み込み、スカラー値（この例では、私たちの簡単なYAMLのフィールド）を出力します。エラーチェックはこの簡単な例では最小限ですが、本番コードではより堅牢であるべきです。

`config.yaml`を使ってプログラムを実行すると、以下の出力が得られます：

```plaintext
値: John Doe
値: 29
値: false
```

## 深い潜入
YAMLは2001年に初めてリリースされ、XMLやJSONといった他のデータ直列化フォーマットよりも読みやすく、ユーザーフレンドリーであるように設計されました。C、Perl、Pythonなどいくつかの言語からデザイン哲学を借用しています。可読性と人間による修正のしやすさで利点がある一方で、インデントに依存することや参照やカスタムタイプなどの広範な特徴セットを含むため、プログラム的に解析することは複雑になり得ます。

`libyaml`はC言語でYAMLを解析し、出力するための堅牢な低レベルアクセスを提供しますが、冗長なAPIのために単純なタスクには煩雑になることがあります。これらの理由から、特にパフォーマンスの高い解析と最小限のコードオーバーヘッドが優先事項である場合、C言語で作業する際にはJSONなどの他のデータ直列化フォーマットや高レベルのライブラリを好むプログラマーもいます。しかし、YAMLは設定ファイルと人間が読みやすいことが最優先の状況において人気の選択肢のままです。TinyYAMLや高レベルインタープリター（例：PythonやLuaの埋め込み）を使用すると、特定のアプリケーションにおいて使いやすさとパフォーマンスニーズのバランスを提供することができます。
