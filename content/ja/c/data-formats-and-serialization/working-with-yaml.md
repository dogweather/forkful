---
title:                "YAMLとの作業"
aliases:
- ja/c/working-with-yaml.md
date:                  2024-02-03T18:13:38.019170-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAMLとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく？

YAMLは、「YAML Ain't Markup Language」の略で、人間が読み書きしやすいデータ直列化標準です。設定ファイルからデータストレージまで、あらゆるアプリケーションに使用できます。プログラマーは、設定ファイルや言語やシステム間のデータ交換のために、読みやすく書きやすいフォーマットが必要な場合に、しばしばYAMLを使用します。

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
