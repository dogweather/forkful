---
title:                "ロギング"
aliases: - /ja/python/logging.md
date:                  2024-01-26T01:09:03.637990-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/logging.md"
---

{{< edit_this_page >}}

## 何となぜ？
ログとは、プログラムが実行されている間にアプリケーションのイベントを記録する過程であり、事後分析やリアルタイム監視のための道筋を残します。プログラマーは問題のデバッグ、パフォーマンスの監視、セキュリティと分析のためのユーザー行動の追跡に役立つためにログを使用します。

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
