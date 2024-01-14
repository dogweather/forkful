---
title:    "Arduino: 정규 표현식 사용하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용해야 할까요?

정규 표현식은 알고리즘을 단순하게 만들어주는 강력한 도구입니다. 이를 사용하면 문자열의 패턴을 찾거나 바꿀 수 있어서 프로그래밍 작업을 더욱 쉽게 만들어줍니다.

# 방법: Arduino에서 정규 표현식 사용하기

정규 표현식을 사용하기 위해서는 `regex` 라이브러리를 설치해야 합니다. 다음의 코드를 통해 간단하게 설치할 수 있습니다.

```Arduino
#include <Regex.h>
```

이제 `regex` 라이브러리를 사용할 준비가 되었습니다! 아래의 예제를 통해 정규 표현식을 사용하는 방법을 알아보겠습니다.

```Arduino
#include <Regex.h>

Regex pattern("my name is (.*)");

String sentence = "Hello, my name is John.";

// 패턴에 일치하는 부분을 찾아서 output에 저장
String output = sentence.match(pattern).group(1);

Serial.println(output); // "John" 출력
```

위의 예제에서는 "my name is" 다음 오는 어떤 단어든지 패턴에 일치하는 부분을 찾아서 출력하는 코드입니다. 이처럼 정규 표현식을 사용하면 특정한 패턴에 따라 문자열을 바꾸거나 추출하는 작업이 쉬워집니다.

# 더 깊이 들어가기

정규 표현식을 사용할 때 조심해야 할 점 중 하나는 정규 표현식의 문법입니다. 정규 표현식은 강력하지만 복잡하고 다양한 문법을 가지고 있기 때문에 사용하기 전에 충분한 연습이 필요합니다. 또한 정규 표현식을 잘 사용하는 방법을 익히기 위해서는 문제를 해결하는 과정에서 자주 사용해보는 것이 중요합니다.

# 또 보기

- [정규 표현식 문법 가이드](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Arduino 레퍼런스 문서](https://www.arduino.cc/reference/en/)
- [ROS에서 정규 표현식 사용하기](https://roboticsbackend.com/ros-regular-expression-how-to-use-it-in-robotics/)