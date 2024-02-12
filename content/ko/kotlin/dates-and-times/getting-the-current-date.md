---
title:                "현재 날짜 가져오기"
date:                  2024-02-03T19:10:28.255528-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래밍에서 현재 날짜를 얻는 것은 개발자들이 자신의 애플리케이션 내에서 현재 날짜에 접근하거나, 표시하거나, 조작할 수 있게 하는 기본적인 작업입니다. 이 기능은 로깅 및 시간 스탬핑 이벤트부터 날짜를 기반으로 한 계산에 이르기까지 모든 것에 중요합니다.

## 방법:

### 표준 코틀린 사용
코틀린은 자체 날짜 및 시간 API가 없으며, 이 기능을 위해 자바 표준 라이브러리에 의존합니다. 다음은 현재 날짜를 얻는 방법입니다:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("오늘의 날짜: $today")
}
```

**샘플 출력:**
```
오늘의 날짜: 2023-04-05
```

### java.util.Date 사용
날짜와 시간 모두가 필요한 작업에는 `java.util.Date`를 사용하는 것이 좋습니다.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("현재 날짜와 시간: $currentDate")
}
```

**샘플 출력:**
```
현재 날짜와 시간: 수요일 4월 05일 15:20:45 GMT 2023
```

### Joda-Time 라이브러리 사용
자바 8이 새로운 날짜 및 시간 API를 소개하기 전에, Joda-Time은 자바와 코틀린에서 날짜-시간 작업의 사실상의 표준이었습니다. 많은 프로젝트에서 더 이상 필요하지 않을 수 있지만, 일부는 여전히 레거시 이유나 개인적인 선호로 인해 사용할 수 있습니다.

프로젝트의 build.gradle 파일에 Joda-Time 라이브러리를 추가하세요:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("오늘의 날짜: $today")
}
```

**샘플 출력:**
```
오늘의 날짜: 2023-04-05
```

### 안드로이드용 ThreeTenABP 사용
안드로이드 개발에 있어서, 안드로이드 API 레벨 26 이전 버전을 위해 자바 시간 API의 백포트인 ThreeTen Android Backport 프로젝트 사용을 권장합니다.

앱의 build.gradle 파일에 의존성을 추가하세요:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Application 클래스에서 초기화하세요:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

그러면 다음과 같이 사용할 수 있습니다:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("오늘의 날짜: $today")
}
```

**샘플 출력:**
```
오늘의 날짜: 2023-04-05
```
