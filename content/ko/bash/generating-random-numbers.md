---
title:                "Bash: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

우리는 프로그래밍을 많이 할 때 랜덤한 숫자를 사용하는 경우가 많습니다. 예를 들어서, 게임을 만들거나 데이터에서 샘플링을 할 때 랜덤한 숫자를 사용합니다. 랜덤한 숫자를 사용하면 다양한 가능성을 가지는 프로그램을 만들 수 있습니다. 그렇지만 어떻게 랜덤한 숫자를 만들 수 있는 걸까요? 이제부터 자세하게 살펴보겠습니다.

## 어떻게

우선, Bash에서는 $RANDOM 변수를 사용하여 랜덤한 숫자를 만들 수 있습니다. 이 변수에는 0부터 32767 사이의 정수가 들어있습니다. 예를 들어서 다음과 같이 코드를 작성할 수 있습니다.

```Bash
echo $RANDOM
```

그리고 출력될 때마다 다른 숫자가 나오는 것을 확인할 수 있습니다.

만약 우리가 원하는 범위의 랜덤한 숫자를 만들고 싶다면 어떻게 해야 할까요? 이때는 명령어를 이용할 수 있습니다. 예를 들어서, 1부터 10 사이의 랜덤한 숫자를 만들고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```Bash
echo $(( $RANDOM % 10 + 1 ))
```

이 명령어는 0부터 9까지의 랜덤한 숫자를 만든 다음에 1을 더하여 1부터 10까지의 숫자를 만들어줍니다.

## 깊이 파고들기

Bash에서 사용할 수 있는 랜덤한 숫자는 $RANDOM 외에도 더 있습니다. 예를 들어서, /dev/random이나 /dev/urandom 파일들을 이용해서 랜덤한 바이너리 데이터를 생성할 수 있습니다. 이러한 파일들은 운영체제가 제공하는 랜덤한 엔트로피를 사용하여 더 강력한 랜덤한 숫자를 생성합니다.

또한, Bash 함수를 이용하여 랜덤한 숫자를 만들 수도 있습니다. 함수를 이용하면 범위가 아닌 지정한 리스트나 배열을 이용하여 랜덤한 숫자를 만들 수 있습니다.

## 이걸로 끝

이제 랜덤한 숫자를 생성하는 법에 대해 알게 되었습니다. 이를 이용하여 다양한 프로그램을 만들어보세요! 다음에 또 보자고 할 수 있도록 링크를 몇 가지 준비해놨습니다.

## 또 보기

- [Bash 셸 프로그래밍](https://ko.wikipedia.org/wiki/Bash)
- [Linux 명령어 목록](https://ko.wikipedia.org/wiki/Linux_%EB%AA%85%EB%A0%B9%EC%96%B4_%EB%AA%A9%EB%A1%9D)
- [Bash를 이용한 랜덤한 숫자 생성 방법](https://zetawiki.com/wiki/Bash_%EB%9E%9C%EB%8D%A4%EC%9D%B8_%EC%88%98%EC%9E%85_%EC%83%9D%EC%84%B1%EB%B0%A9%EB%B2%95)