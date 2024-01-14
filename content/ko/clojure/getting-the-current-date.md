---
title:    "Clojure: 현재 날짜 가져오기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜
현재 날짜를 구하는 작업에 참여하는 이유는 무엇일까요? 매일 날짜를 알아야 할 필요가 있을 때, 또는 특정 날짜 계산을 해야 할 때 마다, 현재 날짜를 얻는 것은 중요합니다. Clojure를 사용한다면 간단한 코드 몇 줄만으로 현재 날짜를 가져올 수 있습니다.

## 방법
Clojure에서 현재 날짜를 가져오는 방법은 매우 간단합니다. `java.time` 라이브러리를 이용하여 `now()` 함수를 호출하면 현재 날짜를 가져올 수 있습니다. 아래의 예제를 참고해보세요.

```Clojure
(require '[java.time :as time])
(time/now)
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
#object[java.time.LocalDateTime 0x44949e13 "2021-09-22T16:35:37.346"]
```

문자열 형태로 날짜를 출력하기 위해서는 `format()` 함수를 사용하면 됩니다. 예를 들어, "YYYY-MM-dd" 형식으로 출력하는 코드는 다음과 같습니다.

```Clojure
(time/format (time/now) "YYYY-MM-dd")
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
"2021-09-22"
```

## 깊게 파기
Clojure에서 날짜와 관련된 작업을 할 때에는 `java.time` 라이브러리를 사용할 수 있습니다. 이 라이브러리는 Java에서 제공하는 `java.time` 패키지를 Clojure에 사용할 수 있도록 감싸주는 역할을 합니다. 따라서 Java에서 사용하는 날짜 관련 클래스와 메소드를 그대로 사용할 수 있습니다. 자세한 내용은 아래의 링크를 참고해보세요.

See Also:
- [Clojure `java.time` documentation](https://clojure.org/reference/java_interop#_java_time)
- [Java `java.time` documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)