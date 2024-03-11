---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:09.216547-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u662F\u6307\u8BC6\
  \u522B\u548C\u8F6C\u6362\u6587\u672C\u5F62\u5F0F\u4E2D\u7684\u4E66\u9762\u65E5\u671F\
  \u5230PowerShell\u80FD\u591F\u7406\u89E3\u548C\u64CD\u4F5C\u7684\u65E5\u671F\u6570\
  \u636E\u7C7B\u578B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u64CD\
  \u4F5C\u3001\u683C\u5F0F\u5316\u3001\u6BD4\u8F83\u6216\u8BA1\u7B97\u65E5\u671F\uFF0C\
  \u8FD9\u4E9B\u5728\u5904\u7406\u65E5\u5FD7\u6587\u4EF6\u3001\u7528\u6237\u8F93\u5165\
  \u6216\u6570\u636E\u5904\u7406\u7684\u811A\u672C\u4E2D\u5F88\u5E38\u89C1\u3002"
lastmod: '2024-03-11T00:14:21.822039-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u662F\u6307\u8BC6\
  \u522B\u548C\u8F6C\u6362\u6587\u672C\u5F62\u5F0F\u4E2D\u7684\u4E66\u9762\u65E5\u671F\
  \u5230PowerShell\u80FD\u591F\u7406\u89E3\u548C\u64CD\u4F5C\u7684\u65E5\u671F\u6570\
  \u636E\u7C7B\u578B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u64CD\
  \u4F5C\u3001\u683C\u5F0F\u5316\u3001\u6BD4\u8F83\u6216\u8BA1\u7B97\u65E5\u671F\uFF0C\
  \u8FD9\u4E9B\u5728\u5904\u7406\u65E5\u5FD7\u6587\u4EF6\u3001\u7528\u6237\u8F93\u5165\
  \u6216\u6570\u636E\u5904\u7406\u7684\u811A\u672C\u4E2D\u5F88\u5E38\u89C1\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么与为什么？
从字符串中解析日期是指识别和转换文本形式中的书面日期到PowerShell能够理解和操作的日期数据类型。程序员这样做是为了操作、格式化、比较或计算日期，这些在处理日志文件、用户输入或数据处理的脚本中很常见。

## 如何做：
PowerShell使用`Get-Date` cmdlet和`[datetime]`类型加速器使从字符串解析日期变得简单，这些对于标准日期格式非常有效。对于更复杂或非标准的日期字符串，可以利用`[datetime]::ParseExact`方法来指定确切的格式。

### 使用`Get-Date`和`[datetime]`:
```powershell
# 使用Get-Date进行简单转换
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**示例输出：**
```
2023年4月1日 星期六 上午12:00:00
```

```powershell
# 使用类型加速器[datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**示例输出：**
```
2023年4月1日 星期六 上午12:00:00
```

### 使用`[datetime]::ParseExact`解析非标准格式：
对于未自动识别的格式，你可以定义确切的格式以确保正确解析。
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**示例输出：**
```
2023年4月1日 星期六 下午2:00:00
```

### 利用第三方库
尽管PowerShell本身对于日期解析非常强大，但对于非常复杂的场景或额外的功能，你可能会探索.NET库例如NodaTime，尽管对于许多典型用例，PowerShell的本地功能将足够。

```powershell
# 仅作为示例使用NodaTime，请注意你需要将库添加到项目中
# Install-Package NodaTime -Version 3.0.5
# 使用NodaTime解析日期
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**示例注意：** 上述代码是概念性示例。实践中，确保正确地将NodaTime添加到你的项目中，以便类型和方法可用。
