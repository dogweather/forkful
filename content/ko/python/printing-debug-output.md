---
title:    "Python: 디버그 출력하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜 디버그 출력을 사용할까요?

디버그 출력은 코드에서 발생하는 문제를 파악하는 데 핵심적인 역할을 합니다. 제대로 설정된 디버그 출력은 무엇이 잘못되었는지 알려주고 프로그래머가 코드를 디버그하기 위한 중요한 정보를 제공합니다.

## 어떻게 디버그 출력을 사용할까요?

Python에서 디버그 출력을 생성하기 위해서는 `print()` 함수를 사용하면됩니다.

```Python
# 코드 예시

def divide(num1, num2):
    print("num1:", num1)
    print("num2:", num2)
    result = num1 / num2
    print("Result:", result)

divide(10, 2)
```

```Python
# 콘솔 출력
num1: 10
num2: 2
Result: 5.0
```

## 깊이있는 디버그 출력에 관하여

디버그 출력은 단순히 코드가 원하는대로 작동하는지 확인하는 것 이상의 역할을 합니다. 여러 개의 디버그 출력을 함께 사용하면 어떤 부분에서 에러가 발생하는지 찾는 데 도움이 됩니다. 또한 디버그 출력을 다른 색상이나 포맷으로 설정하여 출력이 더 쉽게 읽히도록 할 수 있습니다.

# 참고자료

- [파이썬 공식 문서 - 디버깅](https://docs.python.org/3/library/debug.html)
- [호주 유니버시티 블로그 - 디버그와 출력코드](https://blogs.unimelb.edu.au/dept-brag/2017/03/06/debugging-outputs-in-python-code/)
- [점프 투 파이썬 책 - 디버깅(번역)](https://wikidocs.net/9526)