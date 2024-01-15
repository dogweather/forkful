---
title:                "디버그 출력 출력"
html_title:           "Python: 디버그 출력 출력"
simple_title:         "디버그 출력 출력"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

때로는 코드 디버깅을 해야 할 때, 프로그래머들은 출력된 결과를 살펴 보는 것이 도움이 됩니다. 디버그 출력을 프린트하는 것은 코드의 문제점을 파악하고 수정하는 데에 있어서 매우 유용합니다.

## 하우 투

```Python
# 디버깅 출력을 세 가지 다른 방식으로 표시하는 예제
statement_1 = "Hello, world!"
statement_2 = "This is a debug statement."
statement_3 = "Something went wrong!"
print(statement_1) # 표준적인 방법으로 출력
print(f"DEBUG: {statement_2}") # 디버그 문구를 앞에 붙여 출력
print("\033[1;31m" + statement_3) # 콘솔에서 디버그 문구를 빨간색으로 출력
```

출력:

```
Hello, world!
DEBUG: This is a debug statement.
Something went wrong!
```

## 딥 다이브

디버깅 출력은 프로그래머들이 코드에서 오류를 찾는 데에 매우 중요한 역할을 합니다. 디버그 출력은 코드를 따라가는 과정에서 어떤 값들이 사용되었는지, 어떤 함수가 호출되었는지 등을 살펴볼 수 있도록 해주기 때문입니다. 이를 통해 프로그래머들은 코드를 더 잘 이해하고 문제를 해결할 수 있게 됩니다.

## 참고

- [Python 디버그 출력하기](https://www.tutorialspoint.com/python/python_debugging.htm)
- [디버그 출력을하는 방법](https://realpython.com/python-debugging-pdb/)
- [파이썬 내장 함수: print()](https://docs.python.org/3/library/functions.html#print)