---
date: 2024-01-26 01:09:03.637990-07:00
description: "\u65B9\u6CD5\uFF1A Python\u306B\u306F\u30ED\u30B0\u8A18\u9332\u7528\u306E\
  \u7D44\u307F\u8FBC\u307F\u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u3042\u308A\u307E\u3059\
  \u3002\u57FA\u672C\u7684\u306A\u8A2D\u5B9A\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\
  \u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.461539-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法：
Pythonにはログ記録用の組み込みモジュールがあります。基本的な設定は以下の通りです：
```Python
import logging

# ログの基本設定
logging.basicConfig(level=logging.INFO)

# ログメッセージ
logging.debug('This is a debug message')
logging.info('今何をしたかの情報')
logging.warning('警告メッセージ')
logging.error('エラーが発生しました')
logging.critical('プログラムが回復不可能です！')
```
このコードを実行すると、次の出力が表示されます（デフォルトのレベルがWARNINGなので、debugとinfoメッセージは表示されません）：
```
WARNING:root:警告メッセージ
ERROR:root:エラーが発生しました
CRITICAL:root:プログラムが回復不可能です！
```
また、コンソールではなくファイルにログを書き込むよう設定することもできます：
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
これでログが'app.log'ファイルに向けられるようになります。

## 深掘り
ログはプログラミングの初期から存在し、システムログはデータを保持する実際のファイル以外の最古の永続的記憶の形態の一つです。歴史は置いておいても、ログの主要な概念はほぼ変わらずに残っていますが、ツールは進化しています。

Pythonの`logging`モジュールは非常に強力で柔軟です。プログラマーが異なるログレベル（DEBUG、INFO、WARNING、ERROR、CRITICAL）を設定し、ログを分類してフィルタリングするのに役立つ機能を備えています。それには階層的なロガーシステムがあり、ロガー間の親子関係を持つことができ、メッセージをチェーン上で伝播させることができます。

代替手段としては、Loguruやstructlogのようなサードパーティライブラリがあり、組み込みのloggingモジュールよりも高度な機能とよりシンプルなインターフェイスを提供します。これらはより魅力的な出力、より良い構造化データのシリアライゼーション、ログ設定を扱うより直感的な方法を提供できます。

実装に関して言えば、アプリケーションの開始時に一度だけログ設定を行うことが重要です。Pythonのログのベストプラクティスに従い、`logging.getLogger(__name__)`を使用してモジュールレベルでの設定を行うことが推奨されます。

通常の状況下でログはアプリケーションの性能に大きな影響を与えるべきではありません。ただし、何がログされるかには注意が必要です：過度に詳細なログ、特にDEBUGレベルでのものは、アプリケーションを遅くさせたり、すぐにログファイルのストレージを埋め尽くす原因になる可能性があります。

## 参照
Pythonのloggingモジュールについてさらに詳しくは、公式のPythonログクックブックを参照してください。素晴らしい例やベストプラクティスが載っています：https://docs.python.org/3/howto/logging-cookbook.html

構造化ログについての詳しい情報と、ログをより情報豊かで分析しやすくする方法については、Loguruのドキュメントが詳しいです：https://loguru.readthedocs.io

また、12ファクターアプリのメソドロジー、特にログに関するセクションにも目を通して、現代的なアプリのログに関する観点を確認すると良いでしょう：https://12factor.net/logs
