---
title:    "Python: 표준 오류에 쓰기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류에 쓰기를 할 이유는 무엇인가요? 간략히 설명하세요.

표준 오류에 쓰기는 프로그래밍에서 디버깅이나 에러 처리에 도움이 됩니다. 어떤 에러가 발생했는지 알려주고, 문제를 해결하는데 도움이 됩니다.

## 어떻게 하나요

이제 파이썬의 ```sys.stderr```를 사용하여 표준 오류에 쓰는 방법을 알아보겠습니다. 아래의 예제 코드를 확인해보세요.

```Python
import sys

# "This is an error message."를 표준 오류에 쓰는 예제 코드입니다.
sys.stderr.write("This is an error message.")
```

위 코드를 실행하고 나면, 표준 오류에 "This is an error message."가 출력됩니다.

## 깊이 파고들기

파이썬에서 표준 오류에 쓰기를 사용할 때, 몇 가지 유의할 점이 있습니다.

- 표준 오류에 쓰는 메세지는 프로그램을 종료하지 않고 계속 실행됩니다.
- 표준 출력(```sys.stdout```)과 달리, 표준 오류에 쓰는 메세지는 콘솔에서 보이지 않을 수 있습니다.
- 보통은 프로그램이 에러를 처리할 때 표준 오류에 쓰는 것이 좋습니다.

## 이어보기

- [Python 공식 문서: sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Python 프로그래밍: 표준 입출력과 오류 처리](https://wikidocs.net/29)
- [30가지 유용한 Python 코드 스니펫](https://www.datacamp.com/community/tutorials/30-python-language-tricks)
- [파이썬 표준 라이브러리](https://github.com/Yeo0/Let-s-Python/blob/master/Basic-Library.md#sys-시스템-모듈)