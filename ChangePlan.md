# Changes
* Rebuilt Autoconfig - Support 7 boards
* Preparation for 256M DDR3 RAM expansion (Card and addressing mechanism built, need to wire up DDR3)
* Change timings, hopefully much better and more stable builds.

# Plans
1. Wire DDR3 to new card
2. Add ethernet support on HPS
   * TAP device, bridged to ethernet
3. Add SANA-II card, hook to tap device on HPS
   * Probably Z2 card, 64K expansion
4. RTG (Retargetable graphics 1920x1080x32)
   * Hardware mouse cursor
   * No blitter for now, probably a useless option
5. Possibility to move to 68030@28mhz
6. (Maybe) AHI
   * Don't see a real need
