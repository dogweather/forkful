---
title:                "C: 「jsonの操作」"
simple_title:         "「jsonの操作」"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-json.md"
---

{{< edit_this_page >}}

# なぜJSONを使うのか？

JSON（JavaScript Object Notation）はコンピューター間でデータをやりとりする際に便利なフォーマットです。これは人間が読みやすく、コードを書くのにも簡単です。そのため、多くのプログラミング言語で広く使用されています。C言語でもJSONを扱うことができ、より効率的にデータを取得できるようになります。

## 使い方

JSONを扱うためには、まず ```#include <stdio.h>``` のように ```stdio.h``` ヘッダーファイルをインポートする必要があります。次に、データを解析しやすいように ```json-c``` ライブラリをインストールします。以下のサンプルコードを参考にしてください。

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
    FILE *fp;
    char buffer[1024];
    struct json_object *parsed_json;
    struct json_object *name;

    // jsonファイルを読み込む
    fp = fopen("data.json", "r");
    fread(buffer, 1024, 1, fp);

    // 読み込んだデータを解析する
    parsed_json = json_tokener_parse(buffer);

    // 解析したデータから指定したキーを取得する
    json_object_object_get_ex(parsed_json, "name", &name);

    // 取得したデータを出力する
    printf("Name: %s\n", json_object_get_string(name));

    fclose(fp);
    return 0;
}
```

上記のコードでは、```data.json``` という名前のjsonファイルから ```name``` キーの値を取得し、出力しています。

## 深堀り

JSONを扱う上で重要なポイントは、データを解析する際にキーと値のペアを正しく指定することです。また、プログラムでデータを変更する場合は、 ```json-c``` ライブラリで提供されている関数を利用する必要があります。詳細な情報は公式ドキュメントを参照してください。

## 参考リンク

- [Official json-c documentation](https://json-c.github.io/json-c/)