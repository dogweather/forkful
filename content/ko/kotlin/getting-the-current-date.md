---
title:    "Kotlin: 현재 날짜 가져오기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜: 현재 날짜를 가져오는 것에 참여하는 이유

현재 날짜를 가져오는 것은 많은 프로그래밍 작업에서 중요한 부분입니다. 예를 들어, 많은 앱에서는 현재 날짜를 화면에 표시하거나 알림을 설정하는 데 사용합니다. 또한 블로그 게시물에서 게시된 날짜를 자동으로 생성하는 데도 사용될 수 있습니다. 이 기능을 구현하기 위해서는 프로그래머가 현재 날짜를 가져오는 방법을 알아야 합니다.

## 해결하는 방법: 코딩 예제와 코드 블록

```Kotlin
fun getCurrentDate(): String {
    val date = Calendar.getInstance().time
    val formatter = SimpleDateFormat("yyyy-MM-dd", Locale.KOREA)
    return formatter.format(date)
}
```

위의 코드 예제는 `getCurrentDate()` 함수를 사용하여 현재 날짜를 가져오는 방법을 보여줍니다. 이 함수는 Kotlin의 `Calendar` 클래스를 사용하여 현재 시스템의 날짜와 시간을 가져옵니다. 그리고 `SimpleDateFormat`을 사용하여 원하는 날짜 형식을 지정하고, 포맷터를 통해 해당 형식의 날짜를 반환합니다. 이제 이 함수를 호출하면 현재 날짜를 다음과 같은 형식으로 가져올 수 있습니다.

```Kotlin
val currentDate = getCurrentDate()
println(currentDate) // 2021-09-13
```

## 깊이 파고들기: 현재 날짜를 가져오는 더 깊은 정보

현재 날짜를 가져오기 위해서는 `Calendar` 클래스를 사용할 수 있지만, 이 클래스는 원하는 시간대를 지정하는 방법이 없기 때문에 로케일에 따라 시스템 설정을 따르게 됩니다. 따라서 다른 시간대의 날짜를 가져오려면 외부 라이브러리를 사용하여 현재 시스템의 설정을 사용자가 지정할 수 있습니다.

또한 `SimpleDateFormat` 클래스를 사용하여 날짜 형식을 지정할 때, `Locale`를 함께 지정하여 다른 언어나 국가의 형식을 적용할 수 있습니다. 예를 들어, `Locale.US`로 설정하면 미국의 날짜 형식이 적용되고, `Locale.FRENCH`로 설정하면 프랑스의 날짜 형식이 적용됩니다.

# 관련 정보

* [Kotlin 공식 문서: Date & Time](https://kotlinlang.org/docs/datetime.html)
* [프로그래머스: Kotlin Calendar 클래스](https://programmers.co.kr/learn/courses/2717/lessons/12084)
* [Java의 Calendar 클래스와 비교한 Kotlin의 Time API](https://woowabros.github.io/experience/2020/07/16/kotlin-datetime-api.html)

# 같이 보기