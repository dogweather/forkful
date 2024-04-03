---
date: 2024-01-26 00:56:03.209646-07:00
description: "\u65B9\u6CD5\uFF1A ."
lastmod: '2024-03-13T22:44:42.448693-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 方法：
```PowerShell
# 例外を処理する基本的なTry-Catch
try {
    # エラーを引き起こす可能性があるコード
    $result = 1 / 0
} catch {
    # エラーが発生した場合の対処方法
    Write-Host "おっと、エラーが発生しました: $_"
}

# カスタムエラーメッセージの出力
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "ファイルが見つかりませんでした。"
}

# 最後のエラーを調べるための$Error変数の使用
```

## 深く掘り下げて
PowerShellはMonadとして初めて登場して以来、長い道のりを歩んできました。エラー処理は時間と共により堅牢になり、C#のような他のプログラミング言語と似た機能を提供するようになりました。`try-catch-finally`構文はそのような他の言語からの影響を受けたものの一つです。それ以前には、スクリプターは条件のチェックや`$Error`自動変数の使用に大きく依存していました。

PowerShellには、終了エラーと非終了エラーという2つの主要なエラータイプがあります。終了エラーは`try-catch`ブロックで捕捉されない限りスクリプトを停止させますが、非終了エラーは`-ErrorAction Stop`を指定しない限り停止させません。この区別は、エラーがスクリプト全体を停止するほどのものか、単に記録して無視できるものかを決める上で不可欠です。

PowerShellのエラー処理では、`finally`ブロックも利用できます。これは何が起ころうとも-エラーが発生したかどうかに関わらず-実行されます。クリーンアップタスクに最適です。

スクリプトを深く掘り下げるときには、さらに細かい制御が可能となり、特定の例外タイプを扱うことができます。

また、古い方法として、例外をスローすることなくエラーをキャプチャするための`-ErrorVariable`パラメータや、最後の操作が成功したかどうかを教えてくれる`$?`変数もあります。彼らは便利なツールですが、しっかりした`try-catch`に比べれば少し整理されていない感じがします。

## 参照
- [about_Try_Catch_Finally](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
