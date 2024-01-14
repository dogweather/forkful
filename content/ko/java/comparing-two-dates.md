---
title:    "Java: 두 날짜 비교하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
오늘 우리는 자바 프로그래밍에서 두 가지 중요한 날짜를 비교하는 방법을 배우게 됩니다. 날짜 비교는 기간 계산, 스케줄링, 정렬 등 다양한 상황에서 유용하게 활용될 수 있습니다.

## 어떻게
자바에서는 두 개의 날짜를 비교하기 위해 `Date` 클래스의 `compareTo()` 메서드를 사용할 수 있습니다. 이 메서드는 두 개의 `Date` 객체를 비교하여 그 결과를 `int` 값으로 반환해줍니다. 반환되는 값에 따라 두 날짜의 관계를 알 수 있습니다.

```
import java.util.Date;

public class CompareDatesExample {

    public static void main(String[] args) {

        Date date1 = new Date(2020, 7, 1); // 2020년 7월 1일
        Date date2 = new Date(2020, 5, 1); // 2020년 5월 1일

        int result = date1.compareTo(date2);

        if (result > 0) { // date1이 date2보다 미래의 날짜일 때
            System.out.println("date1이 date2보다 미래의 날짜입니다.");
        } else if (result < 0) { // date1이 date2보다 과거의 날짜일 때
            System.out.println("date1이 date2보다 과거의 날짜입니다.");
        } else { // 두 날짜가 같을 때
            System.out.println("두 날짜가 같습니다.");
        }
    }
}
```

위의 예시 코드에서 `Date` 객체에 `2020`이라는 년도를 직접 입력하고 있습니다. 이와 유사하게 `Calendar` 클래스를 사용하여 현재 날짜를 가져올 수도 있습니다. 또한 `SimpleDateFormat` 클래스를 사용하면 `String` 형식으로 된 날짜 값을 `Date` 객체로 변환할 수 있습니다.

```
import java.util.Calendar;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.text.ParseException;

public class DateConversionExample {

    public static void main(String[] args) {

        // 현재 날짜 가져오기
        Calendar calendar = Calendar.getInstance();
        Date currentDate = calendar.getTime();

        // 현재 날짜를 "yyyy-MM-dd" 형식으로 변환
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        String strCurrentDate = dateFormat.format(currentDate);
        System.out.println("현재 날짜: " + strCurrentDate);

        // 날짜 값을 "2020-08-01" 형식으로 변환
        String strDate = "2020-08-01";
        try {
            Date date = dateFormat.parse(strDate);
            calendar.setTime(date);
            System.out.println("입력한 날짜: " + dateFormat.format(date));
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

위의 예시 코드에서는 `Calendar` 클래스를 사용하여 현재 날짜를 가져오고, `SimpleDateFormat` 클래스를 사용하여 날짜 값을 원하는 형식으로 변환하고 있습니다.

## 깊게 들어가기
`compareTo()` 메서드는 두 개의 `Date` 객체의 날짜 값을 비교하기 때문에 해당 날짜의 시간 값은 무시됩니다. 따라서 시간 값을 포함하여 비교하기 위해서는 `Calendar` 클래스를 사용하여 각각의 필드 값을 비교해야 합니다.

또한 자바 8부터는 `LocalDate` 클래스를 사용하여 더 쉽게 날짜 값을 비교할 수 있게 되었습니다. `LocalDate` 클래스는 시간 값이 아닌 날짜 값만을 다루기 때문에 날짜 값의 비교에 최적화되어 있습니다.

## 참고 자료
- [`Date` 클래스 API