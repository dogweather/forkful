---
title:                "yamlを使用する。"
html_title:           "C: yamlを使用する。"
simple_title:         "yamlを使用する。"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜYAMLを使うのか

YAMLは、プログラマーの間で人気のあるファイル形式です。その人気の理由は、人間が読み書きしやすく、コンピューターも解析しやすいことにあります。YAMLを使うことで、よりシンプルで分かりやすいコードを作ることができます。

## 使い方

YAMLをC言語で扱うには、libyamlライブラリをインストールする必要があります。下記のようなコードを書くことで、YAMLファイルを読み込み、データを取得することができます。

```C
#include <stdio.h>
#include <yaml.h>

int main() {

    // YAMLファイルを指定して開く
    FILE *file = fopen("sample.yml", "r");

    // ハンドラを初期化
    yaml_parser_t parser;
    yaml_parser_initialize(&parser);

    // ファイルをハンドラに設定
    yaml_parser_set_input_file(&parser, file);

    // ドキュメントを読み込む
    while (!yaml_parser_parse(&parser, &event)) {

        // イベントを取得
        yaml_event_t event;
        if (!yaml_parser_emit(&parser, &event)) {
            printf("Error: %s\n", parser.problem);
            break;
        }

        // イベントのタイプを判定
        switch (event.type) {
        
            // スカラーの場合
            case YAML_SCALAR_EVENT:
                printf("Key: %s\n", event.data.scalar.value);
                break;
                
            // マッピングのキーの場合
            case YAML_MAPPING_START_EVENT:
                printf("Value: ");
                break;
        }

        // イベントを破棄
        yaml_event_delete(&event);
    }

    // ハンドラを解放
    yaml_parser_delete(&parser);

    // ファイルを閉じる
    fclose(file);

    return 0;
}
```

上記のコードを実行すると、YAMLファイルの内容がキーと値の形式で出力されます。

## さらに深く理解する

YAMLは、マルチラインのデータを表現することができるため、大量のデータを扱う場合でも非常に便利です。また、コードと同じファイル形式でデータを管理できるため、設定ファイルやメッセージリソース、またはデータモデルとしても利用することができます。詳細な情報やYAMLの仕様については、公式ドキュメントを参照してください。

## 参考リンク

- [libyaml](https://github.com/yaml/libyaml)
- [Official YAML Documentation](https://yaml.org/spec/1.2/spec.html)
- [YAMLで始めるC言語プログラミング](https://thinkit.co.jp/article/9720)