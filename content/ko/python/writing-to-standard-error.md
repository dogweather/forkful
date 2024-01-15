---
title:                "표준 오류에 쓰는 방법"
html_title:           "Python: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

누군가는 표준 오류를 써서 작성하게 되는 이유를 알아보겠습니다.

표준 오류 스트림은 프로그램에서 오류 메시지를 캡처하는 중요한 방법입니다. 이를 통해 오류가 발생한 원인을 추적하고 디버깅할 수 있습니다.

## 사용 방법

아래의 예제를 통해 표준 오류를 캡처하는 방법을 살펴보겠습니다.

```Python
import sys
print("Hello, World!", file=sys.stderr)
```

위 코드를 실행하면 "Hello, World!"가 표준 오류 스트림에 출력되고, 콘솔에는 아무것도 출력되지 않습니다.

```Python
import sys
x = 10 / 0 # ZeroDivisionError 발생
print("Oops, something went wrong!", file=sys.stderr)
```

위 코드를 실행하면 ZeroDivisionError가 발생하고, 해당 오류 메시지는 표준 오류 스트림에 출력됩니다. 이를 통해 오류가 발생한 부분을 쉽게 찾을 수 있습니다.

## 깊이 파고들기

표준 오류 스트림은 프로그램에서 오류를 처리하는 데에만 사용되는 것은 아닙니다. 예를 들어, 로깅 시스템에서 오류 메시지를 표준 오류 스트림으로 출력하여 사용자에게 보여줄 수 있습니다.

반대로, 표준 출력 스트림은 프로그램에서 중요한 메시지를 캡처하는데 사용됩니다. 따라서 적절하게 사용하지 않으면 사용자는 중요한 정보를 놓치게 될 수 있으므로 조심해야 합니다.

## 더 알아보기

- [PEP 3145 - PEP 3145 -- 새로운 표준 입력 스트림 기능 추가](https://www.python.org/dev/peps/pep-3145/)
- [Python 표준 입출력 문서](https://docs.python.org/3/library/io.html#module-io)
- [표준 오류 스트림을 활용한 간단한 예제 - Real Python 블로그](https://realpython.com/python-logging/#handling-exceptions-with-logging)

## 참고 문헌

- [Python 입출력과 예외 처리 문서](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [표준 입출력 스트림 - 파이썬 코딩 도장](https://dojang.io/mod/page/view.php?id=2308)
- [Python 로깅 - 파이썬 코딩 도장](https://dojang.io/mod/page/view.php?id=2447)