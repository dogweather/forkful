---
title:                "「未来または過去の日付の計算」"
aliases:
- ja/vba/calculating-a-date-in-the-future-or-past.md
date:                  2024-02-01T21:49:37.494119-07:00
model:                 gpt-4-0125-preview
simple_title:         "「未来または過去の日付の計算」"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
将来または過去の日付を計算することは、指定された日付から特定の日数、月数、または年数が経過した日付を決定することを含みます。プログラマーは、さまざまなアプリケーションでリマインダー、サブスクリプション、有効期限、およびスケジューリング タスクを自動化するために、この機能がよく必要になります。

## 方法
Visual Basic for Applications (VBA) において、将来または過去の日付を計算するために主に使用される関数は `DateAdd()` です。この関数は、指定された時間間隔を日付に加え、新しい日付を返します。

ここに現在の日付に10日を加える基本的な例を示します：

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' 現在の日付に10日を加える
Debug.Print futureDate ' 何かのように出力されます: 04/20/2023
```

同様に、過去の日付を10日見つけるには：

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' 現在の日付から10日を引く
Debug.Print pastDate ' 出力: 03/31/2023, 今日が04/10/2023だと仮定すると
```

これらの例は非常に簡単です。 ` "d"` を `"m"`（月用）や `"yyyy"`（年用）などの他の間隔コードと置き換えることで、異なるタイプの日付計算を実行できます。将来の日付を1年計算する方法は以下の通りです：

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' 現在の日付に1年を加える
Debug.Print nextYear ' 出力: 04/10/2024 今日が04/10/2023である場合
```

## 深掘り
`DateAdd` 関数は、その前身であるBASICから派生して以来、VBAの基本的な部分でした。日付から時間間隔を加算または減算するための簡単さを提供する一方で、VBA（その日付処理機能を含む）が新しいプログラミング言語で見られる便利さや効率性にいつも一致するわけではないことに注意することが重要です。

たとえば、`datetime` モジュールを持つPythonや、`moment.js` や `date-fns` などのライブラリを持つJavaScriptなど、現代の言語は日付操作に対してより直感的で強力な方法を提供します。これらのオプションは、地域設定、タイムゾーン、閏年に対するより良いサポートを提供し、正確な日付計算を全世界規模で必要とするアプリケーションにとって、より適したものになり得ます。

しかし、ExcelマクロやMicrosoft Officeエコシステム内での統合が必要なアプリケーションでは、VBAは実用的な選択肢として残ります。Excelデータに直接アクセスして操作するための簡潔さは、大きな利点です。さらに、スケジューリングやリマインダーのようなほとんどの基本的な日付計算について、VBAの`DateAdd()`は、十分かつ簡潔な解決策を提供します。その構文は新参者にとっても理解しやすく、幅広いOfficeスイートアプリケーションへの統合により、特定の使用例での関連性が保証されています。

結論として、代替のプログラミング言語が日付計算に対するより現代的なアプローチを提供するかもしれませんが、VBAの`DateAdd()`は、最も必要とされる領域での言語の持続力の証です。
