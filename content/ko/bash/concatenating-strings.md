---
title:    "Bash: 문자열 연결하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜

스트링을 연결(concatenating)하는 일은 프로그래밍에서 자주 사용되는 중요한 작업입니다. 여러 개의 스트링을 한 곳에 모아서 기능적으로 하나의 스트링으로 만들 수 있기 때문에, 우리는 스트링을 조작하고 다양한 데이터를 효율적으로 처리하기 위해 스트링 연결을 사용합니다.

## 하는 방법

스트링을 연결하는 방법은 매우 간단합니다. 우선, 우리는 `+` 연산자를 사용하여 스트링을 합칠 수 있습니다. 다음의 예시를 보겠습니다:

```Bash
str1="Hello"
str2="World"
echo $str1$str2
```

위의 코드를 실행하면 `HelloWorld`라는 스트링이 출력됩니다. 또는 우리는 `+=` 연산자를 사용하여 변수에 스트링을 추가할 수도 있습니다. 다음의 예시를 보겠습니다:

```Bash
str="Hello"
str+="World"
echo $str
```

위의 코드를 실행하면 `HelloWorld`라는 스트링이 출력됩니다. 마지막으로, 우리는 `[]`를 사용하여 스트링을 연결할 수도 있습니다. 다음의 예시를 보겠습니다:

```Bash
str1="Hello"
str2="World"
echo "${str1}${str2}"
```

위의 코드를 실행하면 `HelloWorld`라는 스트링이 출력됩니다.

## 더 깊게

스트링을 연결하는 방법은 다양한 방법이 있습니다. 우리가 앞서 살펴본 `+`, `+=`, `[]` 외에도 `printf`를 사용하여 스트링을 연결할 수 있습니다. `printf`는 형식화된 출력을 할 때 사용하는 명령어로, 다음과 같이 사용할 수 있습니다:

```Bash
str1="Hello"
str2="World"
printf "%s%s" $str1 $str2
```

위의 코드를 실행하면 `HelloWorld`라는 스트링이 출력됩니다.

## 연관된 정보

보다 자세한 내용과 다른 스트링 조작 방법을 알고 싶으시다면 다음 링크들을 참고해보세요:

* <https://www.tldp.org/LDP/abs/html/string-manipulation.html>
* <https://linuxize.com/post/bash-concatenate-strings/>