---
title:                "Fish Shell: 랜덤 숫자 생성"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜: 무작위 숫자를 생성하는 이유

무작위 숫자를 생성하는 것은 프로그래밍에서 매우 유용합니다. 예를 들어, 게임에서 랜덤한 이벤트를 발생시키거나, 데이터를 무작위로 샘플링하거나, 보안을 강화하기 위해 무작위 암호를 생성할 수 있습니다. 또한 머신러닝에서 데이터를 무작위로 섞는 등 다양한 용도로 사용될 수 있습니다.

## 방법: Fish Shell을 사용하여 무작위 숫자 생성하기

무작위 숫자를 생성하는 것은 Fish Shell에서 매우 간단합니다. ```math``` 함수를 사용하여 원하는 범위의 숫자를 생성할 수 있습니다. 예를 들어, 1부터 10까지의 숫자 중 무작위로 한 숫자를 출력하려면 다음과 같이 코드를 작성할 수 있습니다.

```Fish Shell 
set random_num (math rand -l 1 10)
echo "무작위 숫자: $random_num"
```

이 코드를 실행하면 매번 다른 숫자가 출력됩니다. 

또한, ```shuf``` 명령어를 사용하여 파일 내의 내용을 무작위로 섞을 수도 있습니다. 다음의 코드를 사용하면 ```numbers.txt``` 파일에 있는 숫자들이 무작위로 섞인 후 출력됩니다.

```Fish Shell 
shuf -n 1 numbers.txt
```

## 깊이 있는 내용: 무작위 숫자 생성 방법

Fish Shell에서는 일반적으로 ```rand``` 함수보다 더 많은 옵션을 제공하는 ```math rand``` 함수를 사용하는 것이 좋습니다. 이 함수를 사용하면 생성할 숫자의 타입, 최소값과 최대값의 범위, 그리고 생성할 숫자의 갯수를 지정할 수 있습니다. 또한, 난수 발생기의 시드값을 설정하여 난수의 패턴을 조절할 수도 있습니다.

또 다른 방법으로는 ```$RANDOM``` 변수를 사용하는 것입니다. 이 변수는 쉘이 실행될 때마다 매번 무작위로 생성되는 숫자를 담고 있습니다. 따라서 이 변수를 사용하여 무작위 숫자를 생성할 수 있습니다.

## See Also

- [Fish Shell 메뉴얼- math rand](https://fishshell.com/docs/current/cmds/math-rand.html)
- [Fish Shell 메뉴얼- $RANDOM](https://fishshell.com/docs/current/cmds/set.html#variable-substitutions)

이제는 Fish Shell을 사용하여 무작위 숫자를 쉽게 생성할 수 있을 것입니다. 다양한 옵션을 활용하여 원하는 방식으로 무작위 숫자를 생성해 보세요!