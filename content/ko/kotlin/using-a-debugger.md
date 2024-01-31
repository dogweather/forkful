---
title:                "디버거 사용하기"
date:                  2024-01-26T03:50:20.559601-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 다루는 것은 코드를 한 단계씩 거치며 기어가 돌아가는 것을 보고 까다로운 버그를 현행범으로 잡는 것에 대한 것입니다. 프로그래머들은 머리카락을 뽑지 않고도 문제가 어디서 발생하는지 알아낼 수 있는 탐정 도구로서 디버거를 사용합니다.

## 사용 방법:
IntelliJ IDEA와 함께 Kotlin에서 디버깅을 경험하는 작은 맛보기입니다 - IDE의 셜록 홈즈:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("숫자를 추측해보세요: ")
        guess = readLine()?.toIntOrNull() ?: continue // 잘못된 입력 무시

        // 'guess'를 작동하는 동안 보려면 여기에 중단점을 설정하세요
        if (guess < mysteryNumber) {
            println("너무 낮아요!")
        } else if (guess > mysteryNumber) {
            println("너무 높아요!")
        }
    }

    println("맞췄어요! 미스테리 숫자는 $mysteryNumber였습니다")
}
```

디버거 출력:
```
숫자를 추측해보세요: 
10
너무 낮아요!
숫자를 추측해보세요: 
50
너무 높아요!
숫자를 추측해보세요: 
42
맞췄어요! 미스테리 숫자는 42였습니다
```

## 깊이 있게 파고들기
디버거는 50년대부터 게임에 있었습니다. 당시에는 상당히 원시적이었고, 디버깅은 소프트웨어보다는 하드웨어에 관한 것일 수 있었습니다. 오늘날 IntelliJ IDEA와 같은 디버거를 사용하면 중단점을 설정하고, 코드 라인을 한 줄씩 짚어보고, 변수의 상태를 여유롭게 조사할 수 있습니다.

IntelliJ의 디버거는 Kotlin에 매우 유용하지만, 유일한 선택지는 아닙니다. Android 개발을 위한 Logcat이나 최소주의자들을 위한 커맨드라인 도구인 jdb와 같은 다양한 대안이 있습니다. 여기서의 묘미는 대부분 JVM 도구 인터페이스(JVMTI)에 관한 것으로, JVMTI는 디버거가 Java 가상 머신과 상호작용하게 하여 Kotlin 개발자들이 정보를 얻을 수 있게 합니다.

## 참고 자료
- IntelliJ IDEA 디버거 문서: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
