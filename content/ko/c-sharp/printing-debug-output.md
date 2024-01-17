---
title:                "디버그 출력 출력하기"
html_title:           "C#: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

자, 우리는 오늘 디버그 출력에 대해 얘기하려고 해요. 그래서 먼저, 디버그 출력이 뭐하는 건지 살펴보고, 왜 프로그래머들이 이를 하는지 알아볼게요.

## 무엇 & 왜? 
디버그 출력이란, 코드에 문제가 있는지 확인하고 싶을 때 추가하는 출력문이라고 할 수 있어요. 이를 통해 코드에서 어떤 값이 저장되는지, 어떤 블록이 실행되는지 등을 확인할 수 있죠. 프로그래머들은 이를 통해 코드를 디버깅하고, 오류를 찾고, 수정하는 데에 큰 도움이 된다고 생각해요.

## 방법:
예를 들어, 우리가 숫자를 더하는 간단한 코드가 있다고 해볼게요. 

```C#
int a = 5;
int b = 10;
int c = a + b;
Debug.WriteLine(c);
```

위와 같이 코드에 `Debug.WriteLine()`을 추가하면, 프로그램이 실행될 때 `c`의 값인 `15`가 출력되게 될 거예요. 만약 우리가 `a`나 `b`의 값을 바꾸면, 새로운 값이 출력되는 것을 확인할 수 있죠. 이런 식으로 디버그 출력을 추가하여 코드의 동작을 확인할 수 있어요.

## 깊이 파고들기:
디버그 출력은 프로그래밍에서 아주 오래된 방법 중 하나에요. 예전에는 출력을 통해 코드를 디버깅하고 문제를 해결하는 방법이었는데, 지금은 디버그 모드와 디버그 도구들이 등장하여 디버깅을 좀 더 쉽고 효율적으로 할 수 있게 됐어요. 또한, 디버그 출력 대신 디버거를 사용하는 방법도 있어요. 이런 도구들을 잘 활용하면 디버그 출력보다 더 강력하고 정확한 디버깅을 할 수 있어요.

## 관련 자료:
디버그 출력에 대해서 더 배우고 싶다면 아래 링크를 확인해보세요.

- [Microsoft 공식 문서](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- [코드가 요긴하게 생각하는 디버그 출력](https://stackoverflow.com/questions/28228450/why-use-debug-writeline-instead-of-debug-write)
- [디버그 출력을 사용하지 않는 다른 방법들](https://stackoverflow.com/questions/170145/c-sharp-when-to-use-debug-and-release-builds)