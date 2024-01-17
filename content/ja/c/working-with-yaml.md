---
title:                "「yamlでの作業」"
html_title:           "C: 「yamlでの作業」"
simple_title:         "「yamlでの作業」"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAMLとは何か？

YAMLは、人間が読みやすくて簡単な構文で構造化データを表現するために使われるフォーマットです。プログラマーは、YAMLを使用して設定ファイルやデータのシリアライズやストレージを行います。

# 方法：

## YAMLデータを読み込む
```C
FILE *file = fopen("data.yaml", "r");
yaml_parser_t parser;
yaml_event_t event;

yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, file);

do {
  yaml_parser_parse(&parser, &event);
  /* 自分でやって */
  yaml_event_delete(&event);
} while (event.type != YAML_STREAM_END_EVENT);

yaml_parser_delete(&parser);
```

## YAMLデータを書き込む
```C
FILE *file = fopen("data.yaml", "w");
yaml_emitter_t emitter;
yaml_event_t event;

yaml_emitter_initialize(&emitter);
yaml_emitter_set_output_file(&emitter, file);

/* 自分でやって */

yaml_emitter_delete(&emitter);
```

# もっと詳しく：

## 歴史的背景
YAMLは、2001年にClark Evansによって開発されました。その後、Oren Ben-KikiとIngy döt Netが開発を引き継ぎ、2006年にに正式なバージョンがリリースされました。YAMLの名前は、「YAML Ain't Markup Language」の略です。

## 代替手段
XMLやJSONなどの他のデータフォーマットもよく使われていますが、YAMLはよりシンプルで人間が読みやすい構文を提供しています。

## 実装の詳細
YAMLは、YAML Ain't Markup Language (YAML)データシリアライザライブラリを使用して実装されています。ただし、これはC言語以外でも利用できるので、他の言語でも使用することができます。

# 関連情報：

- [YAML公式サイト](https://yaml.org/)
- [YAML Ain't Markup Language (YAML)データシリアライザライブラリ](https://github.com/yaml/libyaml)
- [YAMLの歴史](https://yaml.org/spec/history/2001-12-10.html)