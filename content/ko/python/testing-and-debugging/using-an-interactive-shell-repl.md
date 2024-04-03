---
date: 2024-01-26 04:17:11.261117-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uD30C\uC774\uC36C\uC758 REPL\uC5D0 \uBC14\
  \uB85C \uC811\uC18D\uD558\uB824\uBA74 \uBA85\uB839 \uC904\uC5D0 `python`\uC744 \uC785\
  \uB825\uD558\uC138\uC694. \uAC70\uAE30\uC5D0\uC11C \uAC04\uB2E8\uD55C \uC5F0\uC0B0\
  \ \uB610\uB294 \uB2E4\uC911 \uC904 \uCF54\uB4DC\uB97C \uD14C\uC2A4\uD2B8\uD574 \uBCF4\
  \uC138\uC694."
lastmod: '2024-03-13T22:44:54.600389-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC774\uC36C\uC758 REPL\uC5D0 \uBC14\uB85C \uC811\uC18D\uD558\uB824\
  \uBA74 \uBA85\uB839 \uC904\uC5D0 `python`\uC744 \uC785\uB825\uD558\uC138\uC694."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 사용 방법:
파이썬의 REPL에 바로 접속하려면 명령 줄에 `python`을 입력하세요. 거기에서 간단한 연산 또는 다중 줄 코드를 테스트해 보세요:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

함수와 즉각적인 피드백으로 실험해 보세요:

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
... 
>>> greet("Alice")
'Hello, Alice!'
```

라이브러리를 사용해보고 실시간으로 그 기능을 탐색하세요:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

빠른 `exit()` 또는 `Ctrl+D` (윈도우에서는 가끔 `Ctrl+Z`)로 나가세요.

## 자세히 알아보기
REPL의 개념은 파이썬에만 국한된 것이 아니며, Lisp만큼 오래되었습니다. 많은 언어들이 코드를 손으로 조작해 볼 수 있는 즉각적이고 상호작용적인 환경을 제공합니다. 네이티브 파이썬 셸에 대한 대안으로는 IPython과 Jupyter Notebook이 있으며, 이들은 향상된 상호작용, 더 많은 기능, 그리고 다른 도구들과의 더 나은 통합을 제공합니다. 파이썬의 표준 REPL은 단순하지만, 복잡한 객체와 멀티스레드 프로그램을 처리하는 파이썬의 전체 파워를 내재하고 있으며, 더 발전된 도구에 존재하는 자동 완성 및 구문 강조와 같은 기능이 부족합니다.

## 참고 자료
- [파이썬 공식 문서 내 인터프리터에 대한 정보](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: 고급 파이썬 셸](https://ipython.org/)
- [Jupyter 프로젝트](https://jupyter.org/)
