---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# PHPで日付文字列を解析する

## 何？＆なぜ？
日付の解析とは、日付を含む文字列から日付データ（例：年、月、日）を抽出することです。文字列から日付を解析することで、データベースの検索や範囲指定、日付計算などの効率的なデータ操作が可能になるため、プログラマーはこれを行います。

## 方法：
```PHP
$orig_date = "2021-10-03";
$date = date_create($orig_date);
echo date_format($date, 'Y-m-d');
```
出力:
```
2021-10-03
```
または
```PHP
$orig_date = "2021-10-03";
$date = new DateTime($orig_date);
echo $date->format('Y-m-d');
```
出力:
```
2021-10-03
```

## ディープダイブ
1)  **歴史**: PHPの初期バージョンでは、`strtotime()`関数がよく使われました。しかし現在では、より柔軟性と信頼性のある`DateTime`クラスが使われます。

2)  **代替**: 新しいPHPのバージョンには、日付解析に対応する追加の手段が用意されています。例えば `date_parse()`関数や、`date_parse_from_format()`関数があります。

3)  **実装詳細**: DateStringを解析するとき、曖昧さを避けるために形式を明確にすることが重要です。PHPでは、ISO 8601日付形式（`YYYY-MM-DD`）が推奨されます。

## 関連情報
* PHP 公式ドキュメント: [DateTime](https://www.php.net/manual/ja/class.datetime.php)
* PHP 日付/時間 関数: [PHP Date/Time Functions](https://www.php.net/manual/ja/ref.datetime.php)