---
title:    "Java: 날짜를 문자열로 변환하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것에 관심이 있으신가요? 날짜와 관련된 정보를 문자열 형태로 다루는 것은 프로그래밍에서 매우 중요한 기능입니다. 예를 들어, 사용자가 날짜를 입력하면 해당 날짜를 다시 출력하는 프로그램을 만든다고 생각해보세요. 이를 위해서는 날짜 데이터를 문자열로 변환해야 합니다. 따라서 날짜를 문자열로 변환하는 방법에 대해 배우는 것이 중요합니다.

## 하우 투

기본적으로 Java의 ```SimpleDateFormat``` 클래스를 사용하면 날짜를 원하는 형식으로 변환할 수 있습니다. 예제 코드를 살펴보면 다음과 같습니다.

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public static void main(String[] args) {
    Date date = new Date();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy년 MM월 dd일");
    String dateString = sdf.format(date);
    System.out.println(dateString);
}
```

위 코드에서는 현재 날짜를 ```Date``` 객체로 생성한 뒤, ```SimpleDateFormat``` 클래스를 사용하여 원하는 형식인 "yyyy년 MM월 dd일"로 변환하였습니다. 이렇게 생성된 문자열을 출력하면 "2020년 10월 20일"과 같은 형식으로 나타납니다.

여러분은 ```SimpleDateFormat``` 클래스를 사용하여 날짜를 다양한 형식으로 변환할 수 있습니다. 예를 들어, "MM-dd-yyyy"와 같은 형식은 "10-20-2020"과 같이 나타낼 수 있고, "MMMM d, yyyy"와 같은 형식은 "October 20, 2020"과 같이 나타낼 수 있습니다.

## 딥 다이브

```SimpleDateFormat``` 클래스는 다양한 날짜 형식을 지원하지만, 더 많은 옵션을 사용하고 싶다면 ```DateTimeFormatter``` 클래스를 사용할 수도 있습니다. 이 클래스는 Java 8부터 지원되며, ```SimpleDateFormat``` 보다 더 유연하게 날짜를 변환할 수 있습니다.

또한, 날짜를 문자열로 변환하는 것뿐만 아니라, 문자열을 날짜로 변환하는 것도 가능합니다. 이를 위해서는 ```parse()``` 메소드를 사용하면 됩니다. 예를 들어, "2020년 10월 20일"과 같은 문자열을 ```SimpleDateFormat``` 클래스로 변환하는 것도 가능합니다.

## 참고 자료

- [Java SimpleDateFormat Documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Java DateTimeFormatter Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Baeldung: Convert Date to String in Java](https://www.baeldung.com/java-date-to-string-conversion)
- [Guru99: Date to String Conversion in Java](https://www.guru99.com/date-string-conversion.html)