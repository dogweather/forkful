---
title:                "CSVファイルの操作"
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
CSVとは、データをカンマで区切って保存するフォーマットのこと。プログラマーは構造が単純で、Excelなど多くのツールで扱えるため、CSVでデータを操作することがよくある。

## How to: (方法)
Fish ShellでCSVデータを扱うには、`string split`や`awk`コマンドを使うのが一般的です。以下にコード例と出力サンプルを示します。

```fish
# "data.csv"から各行を読み込んで処理する例
cat data.csv | while read -l line
    # カンマで分割して配列に格納
    set -l columns (string split "," $line)
    # 配列の要素を表示
    for column in $columns
        echo $column
    end
end
```

出力例:
```
名前
住所
電話番号
```

```fish
# AWKを使ってCSVの特定の列を抽出する例
awk -F, '{print $2}' data.csv
```

出力例:
```
住所
```

## Deep Dive (深掘り)
CSVは1972年にIBMで開発され、簡単で柔軟性のあるデータフォーマットとして広く利用されている。代替としてJSONやXMLがあるが、CSVはシンプルさで一定のニーズを満たしている。Fish ShellでCSVを扱う際は、バイルトイン関数とUnix系ツールの組み合わせが鍵となる。

## See Also (その他関連情報)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [awk man page](https://man7.org/linux/man-pages/man1/awk.1p.html)
- [GNU Coreutils: `cut` command](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [RFC 4180 - Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)