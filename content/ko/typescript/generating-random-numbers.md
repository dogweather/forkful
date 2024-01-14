---
title:    "TypeScript: 난수 생성하기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# 왜 랜덤 숫자를 생성해야 할까?

랜덤 숫자 생성은 컴퓨터 프로그램에서 매우 중요한 부분입니다. 예를 들어, 게임에서 세 개의 주사위를 굴릴 때, 각각의 주사위가 나올 수 있는 모든 숫자들은 총 6가지가 있습니다. 이렇게 많은 가능한 경우의 수에서 랜덤 숫자를 생성해야 하는데, 그렇지 않으면 게임은 예측 가능해지고 지루해질 것입니다. 또는 추첨 대회에서 당첨을 결정할 때, 우리는 매번 같은 숫자를 고르는 것보다 더 공평하고 흥미로운 방식을 원할 것입니다. 이러한 이유로 랜덤 숫자 생성은 프로그래밍에서 매우 필수적인 요소입니다.

## 어떻게 하면 랜덤 숫자를 생성할 수 있을까?

TypeScript에서 랜덤 숫자를 생성하는 방법은 간단합니다. 먼저, Math 라이브러리에서 제공하는 random() 메소드를 사용합니다. 이 메소드는 0 이상이고 1 미만의 임의의 실수를 반환합니다. 그리고 이 숫자를 우리가 원하는 범위로 변환하기 위해 일부 계산을 해야 합니다. 만약 우리가 1부터 100 사이의 정수를 원한다면, 반환된 실수에 100을 곱하고 1을 더해서 최종 결과를 얻습니다.

```TypeScript
// 1부터 100 사이의 랜덤 숫자 생성
let randomNumber = Math.random() * 100 + 1;
console.log(randomNumber);
// 예시 출력: 74.25323930598379
```

## 더 깊게 들어가보자

Math.random() 메소드는 실제로 유사난수(pseudorandom)를 생성합니다. 이는 사람이 수학적인 공식을 사용하여 예측할 수 없는 숫자를 생성한다는 의미입니다. 하지만 컴퓨터는 결국 이러한 공식을 실행할 수 있기 때문에 완전한 랜덤 숫자는 생성할 수 없습니다. 그래서 우리는 컴퓨터에서 랜덤 숫자를 생성할 때 항상 유사난수를 사용하게 됩니다.

또한, TypeScript에서는 Random 라이브러리를 사용하여 다양한 랜덤 숫자 생성 방법을 구현할 수 있습니다. 예를 들어, Random.integer(min, max) 메소드를 사용하면 최솟값과 최댓값 사이의 랜덤 정수를 생성할 수 있습니다. 그리고 Random.string(length, chars) 메소드를 사용하면 지정한 길이와 문자열에서 랜덤한 문자열을 생성할 수 있습니다.

## 더 알아보기

만약 더 많은 랜덤 숫자 생성 방법을 알고 싶다면 다음 링크를 참고해보세요.

# 더 자세한 정보

[TypeScript 공식 문서 - Math 라이브러리](https://www.typescriptlang.org/docs/handbook/stdlib.html#math) <br>
[TypeScript 공식 문서 - Random 라이브러리](https://www.typescriptlang.org/docs/handbook/maths.html#rand) <br>
[DeveloperHuddle - Math.random() vs Random 정수 생성](https://developerhuddle.tistory.com/176) 
 
# 관련 링크
* [TypeScript 공식 사이트](https://www.typescriptlang.org/)
* [TypeScript 한국 사용자 포럼](