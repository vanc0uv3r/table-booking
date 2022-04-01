# Table Booking
Система бронирования столиков, написанная на Haskell

# Installation

```
git clone https://github.com/vanc0uv3r/table-booking/ && cd table-booking
stack run
```

# Usage
Изначально программа предлагает выбрать режим работы: сразу перейти к бронированию как обычный пользователь, либо же перейти в панель администрирования.    
В панели администрирования доступны следующие действия:
- Забронировать столик вручную (для этого понадовится Имя, телефон и количество персон)
- Отменить бронь по номеру телефону (предполагается, что номер телефона уникален)
- Проинициализировать расписание, начиная с сегоднешнего дня (предыдущее расписание стирается)
- Добавить новые дни в расписание (не затирает предыдущее расписание)

# Config
В проекте присутствует файл конфигурации программы. Список конфигурируемых параметров:
- Количество столиков на одну единицу времени
- Время открытия заведения (в формате HH:MM:SS)
- Время закрытия заведения (в формате HH:MM:SS)
- Интервал между бронированием столика
- Длительность бронирония
- Название файла для хранения информации о столиках (по умолчанию это "tables.txt")
- Пароль для доступа в панель администратора
