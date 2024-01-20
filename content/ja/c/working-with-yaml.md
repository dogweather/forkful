---
title:                "YAMLを扱う"
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLはデータを表すためのフォーマットです。設定ファイルやデータ交換に使用され、JSONよりも読みやすく、シンプルな構造が特徴です。

## How to: (やり方)
C言語には標準のYAMLパーサーはありませんが、libyamlというライブラリを使うことができます。以下に基本的なYAMLファイルの読み込み例を示します。

```C
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *fh = fopen("example.yaml", "r");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if (fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT)
            printf("Value: %s\n", event.data.scalar.value);

        if (event.type != YAML_STREAM_END_EVENT)
            yaml_event_delete(&event);
        else
            break;
    }

    yaml_event_delete(&event);
    yaml_parser_delete(&parser);
    fclose(fh);
    return 0;
}
```

このコードは`example.yaml`ファイルを読み込み、中の値を印刷します。実際の出力はYAMLの内容に依存します。

## Deep Dive (深堀り)
YAMLは2001年に登場し、INI、JSON、XMLなどのフォーマットが存在する中、その可読性と簡潔さで注目を集めました。C言語のlibyamlライブラリは、速度と安定性に重点を置いて設計されていますが、他にもYAMLパーサのライブラリが存在します。YAMLは階層的なデータ構造をうまく扱い、複雑なデータ構造をシンプルなテキスト形式で表現できるため、設定ファイルやアプリケーションのデータ保存に適しています。

## See Also (関連情報)
- YAML公式サイト: https://yaml.org
- LibYAML GitHubリポジトリ: https://github.com/yaml/libyaml
- YAML 1.2 仕様 (和訳): https://yaml.org/spec/1.2/spec.html

この記事で取り上げた内容をさらに学びたい場合は、上記のリンクを参照してください。