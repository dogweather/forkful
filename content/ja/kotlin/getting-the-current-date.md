---
title:    "Kotlin: 現在の日付を取得する"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ日付を取得する必要があるのか

プログラミングをする上で、現在の日付を取得することはよくあることです。例えば、あなたが作成したアプリケーションにユーザーがログインしたときに、そのログインした日付を記録したい場合などです。また、ファイルを作成するときにそのファイルの作成日を表示するような機能を追加したいときにも必要になるかもしれません。

## 日付を取得する方法

日付を取得する方法は簡単です。Kotlinの標準ライブラリには、日付を取得するための便利なクラスや関数が用意されています。例えば、`Date()`コンストラクタを使用することで、現在の日付を取得することができます。

```
Kotlin val currentDate = Date()
println(currentDate)
```
```
出力結果: Thu Jun 17 22:29:12 JST 2021
```

また、より詳細な日付情報を取得したい場合は、参照するフォーマットを指定することもできます。以下の例では、`SimpleDateFormat`クラスを使用して、年月日のフォーマットに変換しています。

```
Kotlin val currentDate = Date()
val simpleDateFormat = SimpleDateFormat("yyyy年MM月dd日")
println(simpleDateFormat.format(currentDate))
```
```
出力結果: 2021年06月17日
```

さらに、時間やタイムゾーンなどの情報も取得することができます。詳細な使い方は、公式ドキュメントを参照してください。

## 詳しい日付の取得

日付の取得には、`Date()`クラスの他にも様々な方法があります。例えば、`Calendar`クラスを使用して、任意の日付情報を取得することも可能です。また、外部ライブラリを使用することで、より高度な機能を実装することもできます。

## 関連リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/datetime.html)
- [JavaのDateクラスとの比較](https://proandroiddev.com/working-with-date-and-timestamp-in-java-and-kotlin-7038848f6d3)

# 同様に見てみる

- [Kotlinで現在の時刻を取得する](https://blog.tamanegi.xyz/blog/2021/06/07/kotlin-time/)