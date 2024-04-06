---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:53.416834-07:00
description: "\u65B9\u6CD5\uFF1A VBA\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\
  \u3001\u901A\u5E38\u3001\u30A8\u30E9\u30FC\u304C\u767A\u751F\u3057\u305F\u3068\u304D\
  \u306BVBA\u306B\u3069\u306E\u3088\u3046\u306B\u9032\u3080\u304B\u3092\u6307\u793A\
  \u3059\u308B`On Error`\u30B9\u30C6\u30FC\u30C8\u30E1\u30F3\u30C8\u3092\u4F7F\u7528\
  \u3057\u3066\u5B9F\u88C5\u3055\u308C\u307E\u3059\u3002\u6700\u3082\u4E00\u822C\u7684\
  \u306A\u30A8\u30E9\u30FC\u51E6\u7406\u6226\u7565\u306B\u306F\u3001`On Error GoTo`\u30E9\
  \u30D9\u30EB\u3001`On Error Resume Next`\u3001\u304A\u3088\u3073`On Error GoTo\u2026"
lastmod: '2024-04-05T21:53:42.793486-06:00'
model: gpt-4-0125-preview
summary: "**\u4F8B1\uFF1A`On Error GoTo`\u3092\u4F7F\u7528\u3059\u308B** \u3053\u306E\
  \u30A2\u30D7\u30ED\u30FC\u30C1\u3067\u306F\u3001\u30A8\u30E9\u30FC\u304C\u767A\u751F\
  \u3057\u305F\u76F4\u5F8C\u306B\u7279\u5B9A\u306E\u30B3\u30FC\u30C9\u30BB\u30AF\u30B7\
  \u30E7\u30F3\u306B\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u8A98\u5C0E\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u306E\u51E6\u7406"
weight: 16
---

## 方法：
VBAでのエラー処理は、通常、エラーが発生したときにVBAにどのように進むかを指示する`On Error`ステートメントを使用して実装されます。最も一般的なエラー処理戦略には、`On Error GoTo`ラベル、`On Error Resume Next`、および`On Error GoTo 0`が含まれます。

**例1：`On Error GoTo`を使用する**

このアプローチでは、エラーが発生した直後に特定のコードセクションにプログラムを誘導することができます。

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' これによりゼロによる除算エラーが発生します

    Exit Sub
ErrHandler:
    MsgBox "エラーが発生しました: " & Err.Description, vbCritical, "エラー！"
    Resume Next
End Sub
```

この例では、実行時エラーが発生すると`ErrHandler`にジャンプし、エラーメッセージを表示してからエラーの次の行で処理を続行します。

**例2：`On Error Resume Next`を使用する**

この戦略では、エラーが発生してもVBAがコードの次の行を実行し続けるように指示します。これは、無害であると予想されるエラーや、実行中に後でエラーを処理する予定の場合に便利です。

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' これによりプログラムが停止することはなく、エラーは無視されます
    
    ' エラーが発生したかどうかを確認する
    If Err.Number <> 0 Then
        MsgBox "エラーが発生しました: " & Err.Description, vbExclamation, "処理されたエラー"
        ' エラーをリセットする
        Err.Clear
    End If
End Sub
```

この場合、プログラムはエラーで停止せず、エラーが発生したかどうかを確認し、発生していれば処理し、その後エラーをクリアします。

## 深掘り
歴史的に見ると、プログラミング言語におけるエラー処理は、単純なgotoステートメントから、JavaやC#のような言語で見られる例外といったより洗練されたメカニズムへと進化してきました。VBAのエラー処理は、現代の例外処理ほど強力または柔軟ではありませんが、Microsoft Office環境でのタスクを自動化する言語のアプリケーションの文脈内でその目的を果たします。

VBAのエラー処理の主な制限は、そのやや煩雑で手動的なアプローチにあり、エラー処理コードの慎重な配置と実行の流れの明確な理解を必要とします。現代のプログラミング言語は、手動でのチェックやコード実行のジャンプを必要とせずにエラー処理コードへの流れを自動的に処理する、try-catchブロックのようなより洗練された解決策を通常提供しています。

それにもかかわらず、VBAのエラー処理メカニズムは、ほとんどの自動化タスクに適しており、適切に使用された場合、処理されていないエラーがユーザーに問題を引き起こす可能性を大幅に減らすことができます。さらに、VBAのエラー処理を理解することは、古いプログラミングパラダイムやソフトウェア開発におけるエラー処理戦略の進化についての洞察を提供することができます。
