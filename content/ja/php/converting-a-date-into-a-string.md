---
title:    "PHP: 日付を文字列に変換する"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

「なぜ日付を文字列に変換するのか」

日付を文字列に変換する理由は多々あります。例えば、データベースから取得した日付データを特定の形式で表示したい場合や、ユーザーが選択した日付を特定の形式で保存したい場合などです。こうした目的を達成するために、日付を文字列に変換する必要があります。

「日付を文字列に変換する方法」

PHPを使って日付を文字列に変換する方法はいくつかあります。以下のコードブロックで、それぞれの方法とその出力をご紹介します。

```PHP
// タイムスタンプを文字列に変換する
$date = time();
echo date("Y/m/d", $date); // 出力：2020/01/01

// 日付オブジェクトをformat()メソッドを使って文字列に変換する
$dateObj = new DateTime();
echo $dateObj->format('Y-m-d'); // 出力：2020-01-01

// strtotime()関数を使って文字列を日付に変換する
$dateStr = "2020年1月1日";
echo date('Y/m/d', strtotime($dateStr)); // 出力：2020/01/01
```

「日付を文字列に変換する際の深掘り」

日付を文字列に変換する際、日付のフォーマットやタイムゾーンによって出力結果が異なることがあります。また、PHPのバージョンによっても出力結果が変わることがあります。より詳しい情報や注意点を知りたい方は、PHPマニュアルの日付と時刻のフォーマットのセクションを確認してください。

「参考リンク」

- PHPマニュアル：日付と時刻のフォーマット - https://www.php.net/manual/ja/function.date.php
- サーバーサイドアカデミー：PHPで日付・時刻を扱う方法まとめ - https://www.s-arcana.co.jp/blog/2019/09/13/11214/
- Qiita：PHPでの日付文字列と日付・時刻の相互変換 - https://qiita.com/u1tnk2r/items/89081112e700ebf1aa8e