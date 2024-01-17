---
title:                "Yamlを扱う。"
html_title:           "PowerShell: Yamlを扱う。"
simple_title:         "Yamlを扱う。"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 何？どうして？

YAMLとは、データフォーマットの一種です。プログラマーが使用する理由は、データの読みやすさと編集のしやすさです。

## 方法：

`PowerShell`コマンドブロック内のコーディング例と出力結果を示します。

### YAMLファイルを読み込む

```
$yaml = Get-Content -Path <yamlファイルパス> | ConvertFrom-Yaml
```

### YAMLデータをコンソールに出力する

```
Write-Output $yaml.<データプロパティ名>
```

## 深堀り：

1. YAMLは、XMLやJSONのような他のデータフォーマットと比較して、より簡潔で人間に読みやすい構文を持っています。
2. YAML以外にも、プログラマーが使用することができるデータフォーマットはたくさんあります。
3. YAMLデータを扱うには、PowerShellに組み込まれた`ConvertTo-Yaml`と`ConvertFrom-Yaml`コマンドレットを使用することができます。

## 関連情報：

- YAMLの公式サイト: https://yaml.org/
- YAMLフォーマットの紹介記事: https://www.stuartellis.name/articles/yaml/
- PowerShellでのYAMLの使用例: https://powershellexplained.com/2017-03-19-Powershell-convertto-yaml/