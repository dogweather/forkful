---
title:                "랜덤 수 생성"
html_title:           "Java: 랜덤 수 생성"
simple_title:         "랜덤 수 생성"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

랜덤 숫자를 생성한다는 것은 무엇인지 2-3문장으로 설명하고, 프로그래머들이 왜 이 작업을 하는지에 대해 설명합니다.

## 무엇이란?

랜덤 숫자를 생성하는 것은 컴퓨터에서 임의의 수를 만드는 것을 말합니다. 이를 통해 프로그래머들은 세상의 다양한 일들을 모델링하고 예측하고 창조할 수 있게 됩니다.

## 왜하는가?

랜덤 숫자 생성은 프로그래밍에서 매우 중요한 작업입니다. 이 작업을 통해 우리는 다양한 알고리즘과 데이터 구조를 테스트할 수 있고, 더 흥미로운 프로그램을 작성할 수 있습니다.

## 방법:

앞으로의 예시와 결과는 모두 Java 언어를 기준으로 합니다. 다른 언어를 사용할 경우 해당 언어의 문법에 따라 조금씩 다르게 작성하셔야합니다.

### 예시 1) 랜덤 숫자 생성

```
java.util.Random rand = new java.util.Random(); //랜덤 인스턴스 생성
int num = rand.nextInt(100); //0에서 100 사이의 랜덤 정수 생성
System.out.println(num); //결과: 34
```

위 코드에서는 Random 클래스의 nextInt() 메소드를 사용해 0에서 100 사이의 랜덤 정수를 생성합니다.

### 예시 2) 랜덤 문자열 생성

```
String[] names = {"James", "John", "Sarah", "Emily"}; //최댓값 정함
java.util.Random rand = new java.util.Random();
int index = rand.nextInt(names.length);
String name = names[index];
System.out.println(name); //결과: Emily
```

위 코드에서는 랜덤 정수를 이용해 배열에서 랜덤으로 값을 가져오는 방법을 보여줍니다.

## 딥 다이브:

### 역사적 배경:

1980년대 이전까지 랜덤 숫자 생성은 매우 복잡한 작업이었습니다. 하지만 1987년에 나온 메르센 트위스터 알고리즘을 통해 랜덤 숫자를 더 쉽게 만들 수 있게 되었습니다.

### 대안:

Java에서는 Random 클래스 외에도 SecureRandom 클래스를 사용해 더 보안적인 랜덤 숫자를 생성할 수 있습니다.

### 구현 세부 사항:

Java의 Random 클래스는 난수 생성을 위해 선형 취합기 방식을 사용합니다. 이 방식은 이전에 생성된 숫자를 바탕으로 새로운 숫자를 만들어냅니다.

## 관련 자료:

- [Java 8 API 문서 - Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [메르센 트위스터 알고리즘 (Wikipedia)](https://ko.wikipedia.org/wiki/%EB%A9%94%EB%A5%B4%EC%84%BC_%ED%8A%B8%EC%9C%84%EC%8A%A4%ED%84%B0_%EC%95%8C%EA%B3%A0%EB%A6%AC%EC%A6%98)