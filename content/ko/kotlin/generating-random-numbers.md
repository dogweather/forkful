---
title:                "Kotlin: 랜덤 숫자 생성"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜
인공적으로 난수를 생성하는 것이 왜 중요할까요? 많은 프로그램들은 사용자의 입력을 통해 작동하고, 때로는 미리 정해진 값들을 사용하기보다는 무작위적인 값이 필요할 수 있습니다. 예를 들어 게임에서 주사위를 굴릴 때, 로또 번호를 무작위로 선택할 때 등에 모두 난수가 필요합니다. 따라서 난수 생성은 많은 프로그램들의 기능에 중요한 역할을 합니다.

# 방법
Kotlin을 사용하여 난수를 생성하는 방법은 간단합니다. ```Random``` 클래스를 사용하면 됩니다. 예를 들어, 1부터 10까지의 무작위 숫자를 출력하려면 다음과 같이 코드를 작성할 수 있습니다.

```Kotlin
val random = Random()
val number = random.nextInt(10) + 1
println(number)
```

여기서 코드를 간단히 설명하면 다음과 같습니다. 우선 ```Random``` 클래스의 인스턴스를 생성합니다. 그리고 ```nextInt()```를 사용하여 0부터 9까지의 난수를 생성하고, 그 결과에 1을 더하여 1부터 10까지의 난수를 만듭니다. 마지막으로 생성된 난수를 출력하면 됩니다.

# 깊은 들여다보기
난수 생성에 대해 더 깊이 알아보겠습니다. 난수는 시스템 시간을 기반으로 생성되며, 일반적으로 해당 시간의 밀리초 단위를 사용합니다. 또한 생성된 난수는 시드 값(seed)을 기반으로 하여 동일한 시드 값을 가지는 경우 항상 동일한 난수가 생성됩니다. 따라서 여러 번 실행해도 같은 난수가 생성되는 것을 방지하기 위해, 보통 시드 값을 변경해줍니다.

# 참고 자료
[Random 클래스 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)\

[난수 생성에 관한 블로그 포스팅](https://medium.com/@mikeandrewlee/random-number-generation-in-kotlin-c4b945c4586d)\

[난수의 기본적인 개념과 사용법 설명 영상](https://www.youtube.com/watch?v=6bH2aVkwojc)\

# 관련 자료들
[코틀린 공식 홈페이지](https://kotlinlang.org/)\

[코틀린 프로그래밍 언어 소개 영상](https://www.youtube.com/watch?v=H_oGi8uuDpA)\

[Java와의 차이점을 설명해주는 블로그 포스팅](https://www.baeldung.com/kotlin/differences-java)\

[온라인 코틀린 코딩 연습 사이트](https://try.kotlinlang.org/)