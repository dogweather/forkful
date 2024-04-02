---
date: 2024-01-26 04:17:11.261117-07:00
description: "REPL(Read-Eval-Print Loop, \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uBC18\
  \uBCF5)\uC740 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uD55C \uBC88\uC5D0 \uD558\uB098\
  \uC529 \uBC1B\uC544\uC11C \uC2E4\uD589\uD55C \uD6C4 \uACB0\uACFC\uB97C \uC0AC\uC6A9\
  \uC790\uC5D0\uAC8C \uBC18\uD658\uD558\uB294 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\
  \uACBD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C\
  \ \uBE60\uB978 \uD14C\uC2A4\uD2B8, \uD559\uC2B5, \uB514\uBC84\uAE45 \uB610\uB294\
  \ \uC989\uC11D\uC5D0\uC11C \uACC4\uC0B0\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.600389-06:00'
model: gpt-4-0125-preview
summary: "REPL(Read-Eval-Print Loop, \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uBC18\
  \uBCF5)\uC740 \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uD55C \uBC88\uC5D0 \uD558\uB098\
  \uC529 \uBC1B\uC544\uC11C \uC2E4\uD589\uD55C \uD6C4 \uACB0\uACFC\uB97C \uC0AC\uC6A9\
  \uC790\uC5D0\uAC8C \uBC18\uD658\uD558\uB294 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\
  \uACBD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C\
  \ \uBE60\uB978 \uD14C\uC2A4\uD2B8, \uD559\uC2B5, \uB514\uBC84\uAE45 \uB610\uB294\
  \ \uC989\uC11D\uC5D0\uC11C \uACC4\uC0B0\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 무엇인가 & 왜 사용하는가?
REPL(Read-Eval-Print Loop, 읽기-평가-출력 반복)은 사용자 입력을 한 번에 하나씩 받아서 실행한 후 결과를 사용자에게 반환하는 프로그래밍 환경입니다. 프로그래머들은 이를 빠른 테스트, 학습, 디버깅 또는 즉석에서 계산을 수행하기 위해 사용합니다.

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
