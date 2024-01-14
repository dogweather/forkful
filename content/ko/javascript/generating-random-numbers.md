---
title:                "Javascript: 랜덤 숫자 생성하기"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜 랜덤 숫자를 생성하는 것인가?
랜덤 숫자를 생성하는 것은 다양한 프로그래밍 작업에서 유용하게 사용될 수 있습니다. 예를 들어, 랜덤 숫자는 게임에서 적이나 아이템의 위치를 정하는 데 사용될 수 있고, 데이터 샘플링이나 A/B 테스트에서도 사용될 수 있습니다.

## 어떻게 하는가?
```Javascript
// 1에서 10까지의 랜덤 숫자 생성
let randomNumber = Math.floor(Math.random() * (10 - 1 + 1) + 1);
console.log(randomNumber);

// 0부터 100까지의 랜덤 숫자 생성
let randomNumber2 = Math.floor(Math.random() * 101);
console.log(randomNumber2);

// 배열에서 랜덤 아이템 선택
let items = ["사과", "바나나", "딸기", "오렌지"];
let randomItem = items[Math.floor(Math.random() * items.length)];
console.log(randomItem);
```

위의 코드는 Javascript의 Math 객체의 random() 메소드를 사용하여 랜덤 숫자를 생성하는 방법을 보여줍니다. 이 메소드는 0 이상 1 미만의 숫자를 반환하므로, 만약 1에서 10까지의 랜덤 숫자를 생성하고 싶다면, 숫자 범위를 조정해야 합니다. 또한, 배열에서 랜덤 아이템을 선택하는 방법은 배열의 길이를 고려하여 랜덤한 인덱스를 생성하고 해당 인덱스의 아이템을 반환하는 방식입니다.

## 더 깊게 알아보기
랜덤 숫자 생성은 일반적으로 컴퓨터의 의사 난수 발생기를 사용합니다. 이 발생기는 초기 값인 seed와 특정 알고리즘에 의해 숫자를 계속 생성하게 됩니다. 따라서 매번 같은 seed를 제공하고 같은 알고리즘을 사용하면, 같은 숫자가 생성됩니다. 이러한 이유로, 보안과 관련된 작업에서는 보다 강력한 알고리즘을 사용하여 의사 난수 발생기를 구현해야 합니다.

## 또 다른 정보 보기
- [Javascript의 Math 객체에 대한 더 자세한 설명](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Javascript에서 랜덤 숫자 생성하기](https://www.w3schools.com/js/js_random.asp)
- [컴퓨터의 의사 난수 발생기에 대한 더 깊은 이해](https://www.random.org/randomness/)