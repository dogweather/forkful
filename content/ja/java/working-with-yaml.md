---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

category:             "Java"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLは人が読み書きしやすいデータ形式です。設定ファイルやメッセージ交換に使われます。JavaでYAMLを扱うことで、設定やデータの読み込みが簡単になります。

## How to (やり方):
JavaでYAMLを扱うためには、ライブラリ`SnakeYAML`の使用が一般的です。以下は基本的な読み込みと書き出しの例です。

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        // YAMLファイル読み込み
        Yaml yaml = new Yaml();
        InputStream inputStream = YamlExample.class
          .getClassLoader()
          .getResourceAsStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
        
        // YAMLファイル書き出し
        Map<String, Object> dataToWrite = Map.of("name", "Yamada", "age", 30);
        String outputYaml = yaml.dump(dataToWrite);
        System.out.println(outputYaml);
    }
}
```

出力例:
```
{ name: Yamada, age: 30 }
```

## Deep Dive (深堀り):
YAMLは2001年に発表され、XMLやJSONの代替として使われてきました。メリットは可読性が高いこと。デメリットはタブを許さないことや、パースが複雑な場合があること。Javaでは他にも`Jackson`や`Gson`などのライブラリがありますが、`SnakeYAML`はシンプルで人気があります。

## See Also (参照):
- SnakeYAMLの公式ドキュメント: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- YAMLの公式仕様: https://yaml.org/spec/1.2/spec.html
- Jacksonライブラリー: https://github.com/FasterXML/jackson
- Gsonライブラリー: https://github.com/google/gson

以上で、JavaでYAMLを扱う基本を解説しました。これをスタートとしてさらなる探求を進めてみてください。
