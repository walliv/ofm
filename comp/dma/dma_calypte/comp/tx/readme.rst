.. _tx_dma_calypte:

TX DMA Calypte
==============

.. vhdl:autoentity:: TX_DMA_CALYPTE

.. toctree::
   :maxdepth: 1
   :caption: Specific subcomponents

   comp/metadata_extractor/readme
   comp/chan_start_stop_ctrl/readme
   comp/packet_dispatcher/readme
   comp/pcie_trans_buffer/readme
   comp/software_manager/readme

General subcomponents
---------------------
* :ref:`mvb_fifox`


----------------
UVM Verification
----------------

.. figure:: img/uvm_ver.jpg
    :align: center
    :scale: 60%


Verification Plan
-----------------

.. list-table:: Tab. 1
    :align: center
    :widths: 5 15 5 5 10 5
    :header-rows: 1

    * - ID
      - Description
      - Requirement level
      - Checked by
      - Status
      - Test name
    * - base
      - Simple packet transfer and check if no packets are droped/damaged on an enabled channel. Randomly
        enable/disable the channels and check the packet counters.
      - Required
      - Func. cover
      - Verified
      - test::base
    * - mult_region
      - For a multiple-regions configuration vary the packet begins/ends in all regions. This puts the
        transaction buffer under stress test.
      - Required
      - Func. cover
      - Unverified
      - test::base/test::speed
    * - pkt_drop
      - Check if packets are droped on a disabled channel. Check if the counters of dropped packets have
        correct values.
      - Required
      - Func. cover
      - Unverified
      - test::base
    * - thrp_meas
      - Measure throughput. Report on the end of the verification in Gbps and GBps. Write the measured values
        to this file.
      - Required
      - None
      - Unverified
      - test::speed
    * - lat_meas
      - Measure latency from input to output. Report average value, maximum, minimum and standard deviation. Repeat
        100000 times and report values in the documentation.
      - Required
      - None
      - Unverified
      - Special sequence that does measurement for a single packet.

Coverage Measure
----------------

There are five tests in the Multiver script.

.. list-table:: test configuration
   :widths: 50 50 50 50 50 50
   :header-rows: 1

   * - conf name
     - Regions
     - Max packet size
     - buffer addres width (DATA, HDR)
     - PCIE LEN (MIN, MAX)
     - channels num

   * - default
     - 1(~40Gb/s)
     - 2^11-1
     - 14-bit, 11-bit
     - 1.256
     - 2

   * - 4_channels
     - 1(~40Gb/s)
     - 2^11-1
     - 14-bit, 11-bit
     - 1.256
     - 4

   * - 8_channels, min_pcie_frames
     - 1(~40Gb/s)
     - 2^11-1
     - 14-bit, 11-bit
     - 1.32
     - 8

   * - buff_size_small
     - 1(~40Gb/s)
     - 2^11-1
     - 13-bit, 10-bit
     - 1.256
     - 2

   * - buff_size_large
     - 1(~40Gb/s)
     - 2^11-1
     - 16-bit, 13-bit
     - 1.256
     - 2


.. list-table:: coverage
   :widths: 50 50 50 50
   :header-rows: 1

   * - conf name
     - base
     - full speed
     - merge

   * - default
     - 75.3494%
     - 74.9002%
     - 75.5762%

   * - 4_channels
     - 76.4729%
     - 76.4729%
     - 76.4729%

   * - 8_channels, min_pcie_frames
     - 77.6599%
     - 77.3954%
     - 77.6599%

   * - buff_size_small
     - 76.2113%
     - 75.7632%
     - 76.4380%

   * - buff_size_large
     - 75.3069%
     - 74.8577%
     - 755337%


Delay is mesured only for the full spead test. This test allways accepts output from DUT (never drops DST RDY). The delay represents how many nanoseconds it takes for a packet to pass through the DMA Calypte.

.. list-table:: delay
   :widths: 50 50 50 50 50
   :header-rows: 1

   * - conf name
     - min
     - max
     - average
     - standard deviation

   * - defaulit
     - 28ns
     - 500ns
     - 175ns
     - 83ns

   * - 4_channels
     - 28ns
     - 816ns
     - 183ns
     - 97ns

   * - 8_channels, min_pcie_frames
     - 24ns
     - 944ns
     - 192ns
     - 111ns

   * - buff_size_small
     - 28ns
     - 500ns
     - 175ns
     - 83ns

   * - buff_size_big
     - 28ns
     - 500ns
     - 175ns
     - 83ns



