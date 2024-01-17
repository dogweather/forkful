---
title:                "날짜를 문자열로 변환하기"
html_title:           "Java: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

날짜를 문자열로 변환하는 것은 프로그래머가 종종 하는 일입니다. 날짜를 문자열로 변환하는 이유는 특정 포맷을 지정하거나 다른 코드와의 호환성을 유지하기 위해서입니다.

# 방법:

날짜를 문자열로 변환하기 위해서는 ```java.text.DateFormat``` 클래스와 그 하위 클래스를 사용합니다. 다음은 ```SimpleDateFormat```을 이용한 예시 코드 입니다:

```
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class StringToDate {
    public static void main(String[] args) {
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        Date date = new Date();
        String dateString = dateFormat.format(date);
        System.out.println(dateString);
    }
}
```

위 코드를 실행하면 현재 날짜가 "yyyy-MM-dd" 형식으로 출력됩니다.

# 심층 분석:

날짜를 문자열로 변환하는 작업은 프로그래밍 분야에서 오래 전부터 이루어졌습니다. ```SimpleDateFormat``` 외에도, ```DateTimeFormatter``` 클래스와 같은 다른 클래스들이 사용될 수 있습니다. 또한, 날짜를 문자열로 변환하는 대신 날짜 객체로 남길 수도 있으며 이렇게 하면 프로그래밍 작업에서 더 쉽고 정확하게 사용될 수 있습니다.

# 참고 자료:

- [Java SimpleDateFormat Document](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java DateFormat and SimpleDateFormat Tutorial](https://www.baeldung.com/java-date-format)
- [Understanding and working with Java SimpleDateFormat](https://www.onooks.com/understanding-and-working-with-java-simpledateformat/)