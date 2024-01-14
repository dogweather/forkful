---
title:    "Java: 将日期转换为字符串"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
在Java编程中，将日期（Date）转换为字符串（String）是非常常见的任务。这可以让我们以用户友好的方式展示日期，也可以用于数据存储和传输。在这篇博文中，我将向您展示如何轻松地在Java中完成这个任务。

## 如何做
```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToString {
    public static void main(String[] args) {
        //创建一个Date对象，以当前日期为例
        Date date = new Date();
        
        //使用SimpleDateFormat类将Date转换为字符串
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        String dateStr = sdf.format(date);
        
        //输出结果
        System.out.println(dateStr);
    }
}
```

输出结果：
```
2021-08-18
```

## 深入了解
在上面的例子中，我们使用了SimpleDateFormat类来将Date对象转换为字符串。这个类使用了一些特殊的字符来定义日期格式，比如"yyyy"代表四位年份，"MM"代表两位月份，"dd"代表两位日期等。您也可以根据自己的需要定义日期格式。另外，SimpleDateFormat类还可以将字符串转换为Date对象，非常灵活实用。

## 参考资料
如果您想深入了解日期和字符串的转换，可以参考下面的链接：
- Java日期类：https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- SimpleDateFormat类：https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html

---

### 参见
- [Java格式化日期：将Date对象转换为字符串](https://www.runoob.com/w3cnote/java-date-to-string.html)
- [Java日期与字符串相互转换](https://blog.csdn.net/zyaxie/article/details/20745035)