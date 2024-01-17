---
title:                "랜덤 숫자 생성하기"
html_title:           "Fish Shell: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
난수 생성은 임의의 숫자를 생성하는 프로그래밍 기술입니다. 프로그래머들은 이를 하지 않아도 되지만, 여러 암호화와 게임에서 사용되고 더 나은 코드를 작성하는 데 유용하기 때문에 이를 사용합니다.

## 어떻게:
Fish Shell에서의 난수 생성 코드는 다음과 같이 작성할 수 있습니다. ```Fish Shell ...``` 내에서 예제 코드와 출력을 확인해보세요.
```
# 난수 생성하기
set my_number (math random)
echo $my_number

# 1에서 10 사이의 난수 생성하기
set my_bounded_number (math random -l 1 -u 10)
echo $my_bounded_number
```

## 더 알아보기:
- 난수 생성은 컴퓨터과학 분야에서 매우 중요한 부분입니다. 예전에는 난수 생성을 위한 물리적인 장치가 사용되었지만 현재는 소프트웨어 방식으로 많이 사용됩니다.
- 다른 언어에서도 난수 생성 기능을 사용할 수 있습니다. Java에서는 ```java.util.Random``` 클래스를 사용하며, Python에서는 ```import random``` 구문으로 사용할 수 있습니다.
- Fish Shell에서는 ```math random``` 기능을 사용하여 난수 생성을 할 수 있습니다. 이는 C언어의 ```rand()``` 함수와 유사합니다.
- 난수 생성은 암호화나 시뮬레이션 등 많은 분야에서 사용됩니다. 이러한 분야에서는 난수가 예측할 수 없고 일관성이 없어야 하기 때문에 난수 생성 기술이 매우 중요합니다.

## 관련 자료:
- [Fish Shell 매뉴얼](https://fishshell.com/docs/current/index.html)
- [다른 언어에서의 난수 생성 기술](https://www.random.org/randomness/)