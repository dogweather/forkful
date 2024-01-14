---
title:    "Fish Shell: 디버그 출력 프린팅"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# 왜
디버그 출력을 프린팅하는 데 왜 관심이 있어야 할까요? 일반적인 개발자들이 자주 사용하는 방식에 대해 알아보고, 디버그 출력이 어떤 이점을 가지는지 알아보겠습니다.

## 어떻게
디버그 출력을 프린팅하는 방법에 대해 알아봅시다. 아래의 예제 코드와 함께 실제 출력 결과를 확인해보세요.

```Fish Shell
# 디버그 출력 설정
set -x

# 출력할 내용 작성
echo "Hello World"

# 출력 결과
+ echo "Hello World"
Hello World
```

위의 예제 코드에서는 `set -x`를 사용하여 디버그 출력을 활성화한 뒤, `echo`를 사용하여 "Hello World"를 출력하고 있습니다. 디버그 출력이 활성화되면 `+` 기호가 출력되며, `echo`로 출력한 내용이 그 아래에 디버그 출력으로 나타납니다. 이를 통해 우리는 프로그램이 실행되는 동안 어떤 일이 일어나고 있는지 쉽게 파악할 수 있습니다.

## 딥 다이브
디버그 출력에 대해 더 자세히 알아봅시다. 디버그 출력을 활성화하면 프로그램이 실행되는 동안 변수의 값을 쉽게 추적할 수 있습니다. 예를 들어, 아래의 코드에서는 `count` 변수의 값을 출력하고 있습니다.

```Fish Shell
set -x
count=1
set -x count

+ count=1
+ count
1
```

또 다른 유용한 사례는 반복문에서 디버그 출력을 사용하는 것입니다. 아래의 코드에서는 `for`문이 한 번 실행될 때마다 `i` 값의 변화를 살펴볼 수 있습니다.

```Fish Shell
set -x
for i in 1 2 3
set -x i

+ for i in 1 2 3
+ set -x i
+ i=1
+ for i in 1 2 3
+ set -x i
+ i=2
+ for i in 1 2 3
+ set -x i
+ i=3
```

디버그 출력은 디버깅을 도와주는 강력한 도구입니다. 프로그램이 어떻게 동작하는지 이해하고, 문제를 해결하기 위해서는 필수적인 기능이기 때문에 자주 사용되는 기능이기도 합니다.

# 참고자료
- [Fish Shell 공식 홈페이지](https://fishshell.com)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell Wiki](https://github.com/fish-shell/fish-shell/wiki)