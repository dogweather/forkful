---
title:                "C#: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

"## 왜 랜덤 수를 생성하는가?"

랜덤 숫자를 생성하는 일은 프로그래머들에게는 매우 중요한 일입니다. 랜덤 수를 사용하면 프로그램을 더 다양하고 혹은 에러를 없앨 수 있습니다. 랜덤 수를 생성함으로써 사용자와 상호작용하고 새로운 기능을 구현할 수 있습니다. 

"## 어떻게 하는가?"

먼저 `using System`을 써서 `Random` 클래스를 허용해야 합니다. 그리고 `Random` 객체를 생성해야 합니다.

```C#
using System;

Random random = new Random();
```

랜덤 수를 생성하기 위해 `Next()` 메소드를 사용할 수 있습니다. 이 메소드는 인자가 없을 경우, 0에서 `Int32` 정수의 최대값 사이의 값 중 하나를 반환합니다.

```C#
int randomNumber = random.Next();
```

만약에 랜덤 수가 특정 범위 사이의 값만 필요하다면, `Next()` 메소드의 첫번째 파라미터에 최소값을, 두번째 파라미터에 최대값을 지정할 수 있습니다. 이렇게 하면, 최소값에서 최대값 사이의 값을 반환할 것입니다.

```C#
int randomNumberInRange = random.Next(1, 10); // 1에서 9 사이의 랜덤 수
```

"## 더 깊게 알아보기"

`Random` 클래스는 무작위 수를 생성하기 위한 여러 가지 메소드들을 제공합니다. 이들은 다양한 데이터 유형을 다룰 수 있도록 오버로딩되어 있습니다. 또한, 랜덤 수를 생성하는 알고리즘을 더욱 정교하게 조정할 수 있도록 다양한 옵션과 설정들을 제공합니다.

예를 들어, 랜덤 수를 생성할 때, 시작점 인덱스와 범위를 지정할 수 있습니다. 또한, 생성된 랜덤 수를 `double`, `decimal`, `float` 등의 다양한 데이터 유형으로 변환할 수 있습니다.

"## 더 읽어보기"

1. Microsoft docs [Random 클래스 웹사이트](https://docs.microsoft.com/ko-kr/dotnet/api/system.random?view=net-5.0)
2. C# 레퍼런스 문서 [Random 클래스 페이지](https://docs.microsoft.com/ko-kr/dotnet/api/system.random?view=net-5.0)