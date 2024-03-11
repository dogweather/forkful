---
date: 2024-01-26 03:50:20.559601-07:00
description: "\uB514\uBC84\uAC70\uB97C \uB2E4\uB8E8\uB294 \uAC83\uC740 \uCF54\uB4DC\
  \uB97C \uD55C \uB2E8\uACC4\uC529 \uAC70\uCE58\uBA70 \uAE30\uC5B4\uAC00 \uB3CC\uC544\
  \uAC00\uB294 \uAC83\uC744 \uBCF4\uACE0 \uAE4C\uB2E4\uB85C\uC6B4 \uBC84\uADF8\uB97C\
  \ \uD604\uD589\uBC94\uC73C\uB85C \uC7A1\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBA38\uB9AC\uCE74\uB77D\
  \uC744 \uBF51\uC9C0 \uC54A\uACE0\uB3C4 \uBB38\uC81C\uAC00 \uC5B4\uB514\uC11C \uBC1C\
  \uC0DD\uD558\uB294\uC9C0 \uC54C\uC544\uB0BC \uC218 \uC788\uB294 \uD0D0\uC815 \uB3C4\
  \uAD6C\uB85C\uC11C \uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.096934-06:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB97C \uB2E4\uB8E8\uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C\
  \ \uD55C \uB2E8\uACC4\uC529 \uAC70\uCE58\uBA70 \uAE30\uC5B4\uAC00 \uB3CC\uC544\uAC00\
  \uB294 \uAC83\uC744 \uBCF4\uACE0 \uAE4C\uB2E4\uB85C\uC6B4 \uBC84\uADF8\uB97C \uD604\
  \uD589\uBC94\uC73C\uB85C \uC7A1\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBA38\uB9AC\uCE74\uB77D\uC744\
  \ \uBF51\uC9C0 \uC54A\uACE0\uB3C4 \uBB38\uC81C\uAC00 \uC5B4\uB514\uC11C \uBC1C\uC0DD\
  \uD558\uB294\uC9C0 \uC54C\uC544\uB0BC \uC218 \uC788\uB294 \uD0D0\uC815 \uB3C4\uAD6C\
  \uB85C\uC11C \uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
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
