---
title:                "Go: 未来や過去の日付の計算"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
日付を過去や未来に計算する理由は、覚えておくことが重要です。例えば、リマインダーやスケジュール作成など、日常生活で非常に役立つ機能です。

## 方法
Goプログラミング言語を使用して日付を計算する方法はとてもシンプルです。以下のコードを参考にしてください。

```Go
// 現在の日付
今日 := time.Now()

// 1ヶ月後の日付を計算する
1ヶ月後 := 今日.AddDate(0, 1, 0)

// 1ヶ月前の日付を計算する
1ヶ月前 := 今日.AddDate(0, -1, 0)
```

上記のコードでは、`time`パッケージの`AddDate()`関数を使用して日付を計算しています。第一引数は年数、第二引数は月数、第三引数は日数を表します。このように、数値を調整することで未来や過去の日付を計算することができます。

## 深堀り
日付を計算する際には、`time`パッケージの他にも様々な方法があります。例えば、`time.Parse()`関数を使用すれば特定の日付型の文字列を解析して日付を取得することができます。また、`time.Date()`関数を使用すれば独自の日付を作成することも可能です。さらに、タイムゾーンやロケールを設定することもできます。これらの機能を上手く組み合わせて使うことで、あなたのプログラムに最適な日付計算方法を見つけることができます。

## 参考リンク
- [Goプログラミング言語公式サイト](https://golang.org/)
- [timeパッケージドキュメント](https://golang.org/pkg/time/)
- [Effective Go日本語訳](https://github.com/golang-jp/golang.org/blob/master/doc/effective_go.ja.md#%E6%97%A5%E4%BB%98%E3%81%AE%E8%A8%AD%E5%AE%9A)
- [Goで日付を計算する方法について学ぼう](https://developer.ibm.com/jp/blogs/try-to-calculate-date-in-golang/)