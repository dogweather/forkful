---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:19:47.436594-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLは、読み書きが簡単なデータシリアライゼーション言語です。その明快さと人間味のおかげで、プログラマーは設定ファイル、シンプルなデータストレージ、言語間のデータ交換に使用します。

## 使い方：
C言語で「tomlc99」ライブラリを使ってTOML設定ファイルを解析しましょう。まず、ライブラリをインストールします。次に、`config.toml`を作成します：

```toml
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

次に、C言語で解析します：

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Error: cannot open config file\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Error: %s\n", errbuf);
        return 1;
    }

    printf("Title: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Owner Name: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
サンプル出力：
```
Title: "TOML Example"
Owner Name: "Tom Preston-Werner"
```

## より深く：
TOMLは、Tom Preston-Wernerによって2013年に作られた、Tom's Obvious, Minimal Languageの略です。XMLやYAMLのような形式に対してよりシンプルな代替案として機能し、人間が読み書きしやすいことに重点を置いています。JSONも別の代替案ですが、TOMLは人間が視覚的に解析しやすい構造を保持しており、これが設定ファイルでの採用の主な理由の一つです。

C言語でTOMLを扱うには、言語がネイティブにサポートしていないため、パーサーライブラリを選択することが関わります。「tomlc99」のようなライブラリはC99に準拠しており、TOMLテキストをデコードするAPIを提供します。パフォーマンスを考慮する場合、適切なエラー処理とメモリ管理が重要です。なぜならC言語は組み込みのガベージコレクションを持っていないからです。

## 参照：
1. TOML仕様: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHubリポジトリ: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. データシリアライゼーションフォーマットの比較: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
