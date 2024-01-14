---
title:    "Java: 「日付を文字列に変換する」"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由を説明します。日付を文字列に変換することで、ある形式の日付を別の形式に変換することができます。例えば、データベースから取得した日付をユーザーが理解しやすい形式に変換することができます。

## 方法
日付を文字列に変換するには、Javaの組み込みのクラスである `SimpleDateFormat` を使用します。以下のコードを使用して、日付を指定した形式の文字列に変換することができます。

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

// 日付の形式を指定する
SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd");

// 今日の日付を取得する
Date date = new Date();

// 日付を指定した形式の文字列に変換する
String dateString = format.format(date);

// 結果を出力する
System.out.println(dateString); // 出力結果: 2021/05/31
```

日付を指定した形式の文字列に変換するために、 `SimpleDateFormat` のコンストラクタに渡す形式を指定する必要があります。上の例では、 `yyyy/MM/dd` という形式で日付を指定していますが、任意の形式を指定することができます。また、日付だけでなく、時刻を含めることもできます。

## 深堀り
`SimpleDateFormat` を使用して日付を文字列に変換する際、注意する必要がある点があります。例えば、日付のフォーマット指定に使用する文字列が間違っていると、日付が正しく変換されない可能性があります。また、特定のロケールやタイムゾーンに対応するために、 `SimpleDateFormat` のコンストラクタに引数を渡すこともできます。詳細な情報はJavaのドキュメンテーションを参照してください。

## 参考
- [Javaの公式ドキュメント - SimpleDateFormatクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/text/SimpleDateFormat.html)
- [Javaの日付と時刻のフォーマット指定文字列](https://www.mlab.im.dendai.ac.jp/~ichikawa/lecture/2011/Java/markup/DateTimeFormat.html)
- [Javaで日付を操作する方法](https://techacademy.jp/magazine/21016)