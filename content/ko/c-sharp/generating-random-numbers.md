---
title:    "C#: 랜덤 숫자 생성"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜 그랜드 랜덤 숫자를 생성하는가?

우리는 모두 컴퓨터 프로그래밍에서 랜덤 숫자 생성이 중요하다는 것을 알고 있습니다. 이것은 개발자가 다양한 상황에 대처할 수 있게 해주는 강력한 도구입니다. 여기서는 C#에서 랜덤 숫자를 생성하는 방법을 살펴보고 실제 코드 예제를 통해 보여줄 것입니다.

## 어떻게 하면 랜덤 숫자를 생성할 수 있나요?

랜덤 숫자를 생성하는 가장 간단한 방법은 `Random` 클래스를 사용하는 것입니다. 이 클래스는 C#에서 기본적으로 제공됩니다. 먼저 `Random` 클래스의 인스턴스를 만듭니다.

```C#
Random rand = new Random();
```

여기서 `rand`는 우리가 사용할 랜덤 숫자 생성기 입니다.

다음으로, `Next()` 메소드를 사용하여 랜덤 숫자를 생성할 수 있습니다. 인자로는 원하는 숫자 범위를 지정할 수 있습니다. 예를 들어, 1에서 100까지의 랜덤 숫자를 생성하고 싶다면 다음과 같이 사용합니다.

```C#
int randomNumber = rand.Next(1, 101);
Console.WriteLine(randomNumber);
```

코드를 실행하면 매번 다른 숫자가 출력되는 것을 확인할 수 있습니다.

```C#
67
23
92
```

이처럼 간단한 방법으로 랜덤 숫자를 생성할 수 있습니다.

## 더 깊이 알아보기

우리는 이제 `Next()` 메소드를 통해 랜덤 숫자를 생성할 수 있지만, 만약 우리가 숫자가 아닌 다른 자료형을 생성하고 싶다면 어떻게 할까요? 이때는 `Next()` 메소드에 다른 오버로드된 버전을 사용하면 됩니다.

예를 들어, `NextDouble()` 메소드는 0 이상 1 미만의 랜덤 실수를 생성합니다.

```C#
double randomDouble = rand.NextDouble();
Console.WriteLine(randomDouble);
```

실행 결과는 다음과 같이 나타납니다.

```C#
0.75892971487462
0.439255462611492
0.98204241185114
```

또한 `NextBytes()` 메소드를 사용하면 `byte` 배열을 생성할 수 있습니다.

```C#
byte[] byteArray = new byte[5];
rand.NextBytes(byteArray);
Console.WriteLine(String.Join(" ", byteArray));
```

출력 결과는 다음과 같습니다.

```C#
123 48 202 17 61
65 239 72 215 227
9 90 79 172 33
```

이처럼 `Random` 클래스는 다양한 메소드들을 제공하여 우리가 다양한 자료형의 랜덤 값을 생성할 수 있게 해줍니다.

## 참고

- [Microsoft 문서 - `Random` 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.random?view=net-5.0)
- [C# Random 클래스의 사용법](https://blog.naver.com/PostView.nhn?blogId=dnet9&logNo=221211571918&categoryNo=0&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=thumbnailList)